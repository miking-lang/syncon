{-# LANGUAGE RankNTypes, KindSignatures, ScopedTypeVariables, TupleSections, NamedFieldPuns, OverloadedLists, DeriveGeneric, LambdaCase, StandaloneDeriving, FlexibleContexts, UndecidableInstances, DataKinds, GeneralizedNewtypeDeriving, GADTs, ViewPatterns #-}

module Text.Earley.Forest.Transformations
( mkGrammar
, mkNNFGrammar
, Rule(..)
, ruleAsTuple
, Sym(..)
, NT(..)
, NtKind(..)
, rightRecursive
) where

import Prelude

import GHC.Generics (Generic)

import Codec.Serialise (Serialise)
import Control.Arrow ((>>>), (&&&))
import Control.DeepSeq (NFData)
import Control.Monad.ST (runST, ST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask, asks)
import Data.Foldable (forM_, toList, fold)
import Data.Function ((&))
import Data.Functor ((<&>), void)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.List (partition)
import Data.List.NonEmpty ()
import Data.Maybe (mapMaybe)
import Data.STRef (STRef, newSTRef, readSTRef, modifySTRef')
import Data.Sequence (Seq((:|>)))
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Data.Sequence as Seq

import Text.Earley.Forest.Grammar (Grammar(..), runGrammar, Prod(..), NodeKind(..))

data NtKind nodeLabel = NtMerge | NtNode nodeLabel | NtNormal deriving (Show, Eq, Generic)
instance (NFData nodeLabel) => NFData (NtKind nodeLabel)
instance (Serialise nodeLabel) => Serialise (NtKind nodeLabel)
newtype NT = NT Int deriving (Show, Eq, Hashable, NFData, Serialise)

-- | The first label is the closest label (i.e., the one used for token label a or
-- nodeLeaf label a). The sequence of labels are applied right first in sequence.
data Sym intLabel nt tok
  = Sym !tok !(Maybe intLabel) !(Seq intLabel)
  | Nt !nt !(Seq intLabel)
  deriving (Show, Generic)
instance (NFData intLabel, NFData nt, NFData tok) => NFData (Sym intLabel nt tok)
instance (Serialise intLabel, Serialise nt, Serialise tok) => Serialise (Sym intLabel nt tok)

-- | 'Rule nt syms is a production with lefthand side 'nt' and righthand side
-- 'syms'.
data Rule nt t sym
  = Rule !nt !(Seq sym)
  | NodeRule !nt !(Seq t) !nt !(Seq t)
  deriving (Show, Generic)
instance (NFData nt, NFData t, NFData sym) => NFData (Rule nt t sym)
instance (Serialise nt, Serialise t, Serialise sym) => Serialise (Rule nt t sym)

ruleAsTuple :: Rule nt t (Sym intLabel nt t) -> (nt, Seq (Sym intLabel nt t))
ruleAsTuple (Rule nt syms) = (nt, syms)
ruleAsTuple (NodeRule nt l nt' r) = (nt, fmap mkSym l <> pure (Nt nt' mempty) <> fmap mkSym r)
  where
    mkSym t = Sym t Nothing mempty

-- | A production (or list of productions) with the given non-terminal. No other
-- production has that non-terminal on the lefthand side.
data WrappedProd intLabel tok
  = WrappedProd NT (Prod' intLabel tok 'Interior)
  | WrappedProds NT [Prod' intLabel tok 'Node]
type Prod' intLabel tok = Prod (WrappedProd intLabel tok) intLabel tok

data MkGrammarState s nodeLabel intLabel tok = MkGrammarState
  { nts :: !(STRef s (Seq (NtKind nodeLabel)))
  , alreadyStarted :: !(STRef s (HashSet NT))
  , rules :: !(STRef s (Seq (Rule NT tok (Sym intLabel NT tok)))) }

-- | Construct a standard context-free grammar from the given 'Grammar'. Note that
-- the non-teminals 'NT' contain additional information about post-processing that
-- should be done on the result(s) derived using the non-teminal: nothing, merging,
-- or constructing a node.
mkGrammar :: forall nodeLabel intLabel tok.
             (forall r. Grammar r nodeLabel intLabel tok (Prod r intLabel tok 'Node))
          -> (NT, Seq (Rule NT tok (Sym intLabel NT tok)), Seq (NtKind nodeLabel))
mkGrammar unfixedGrammar = runST $ do
  initialState@MkGrammarState{rules, nts} <- MkGrammarState <$> newSTRef Seq.empty <*> newSTRef S.empty <*> newSTRef Seq.empty
  startNt <- runReaderT (runGrammar mkRule mkAmbig unfixedGrammar >>= mkStart) initialState
  (startNt,,) <$> readSTRef rules <*> readSTRef nts
  where
    freshNt :: NtKind nodeLabel -> ReaderT (MkGrammarState s nodeLabel intLabel tok) (ST s) NT
    freshNt k = do
      ref <- asks nts
      prev <- lift $ Seq.length <$> readSTRef ref <* modifySTRef' ref (:|> k)
      return $ NT prev

    addProd :: Rule NT tok (Sym intLabel NT tok) -> ReaderT (MkGrammarState s nodeLabel intLabel tok) (ST s) ()
    addProd rule = do
      ref <- asks rules
      void $ lift $ modifySTRef' ref $ (:|> rule)

    mkRule :: forall s. nodeLabel -> Prod (WrappedProd intLabel tok) intLabel tok 'Interior
           -> ReaderT (MkGrammarState s nodeLabel intLabel tok) (ST s) (Prod' intLabel tok 'Node)
    mkRule label p = do
      nt <- freshNt $ NtNode label
      return $ NonTerminal mempty (WrappedProd nt p) mempty
    mkAmbig :: forall s. [Prod (WrappedProd intLabel tok) intLabel tok 'Node]
            -> ReaderT (MkGrammarState s nodeLabel intLabel tok) (ST s) (Prod' intLabel tok 'Node)
    mkAmbig ps = do
      nt <- freshNt NtMerge
      return $ NonTerminal mempty (WrappedProds nt ps) mempty

    mkStart :: Prod' intLabel tok 'Node -> ReaderT (MkGrammarState s nodeLabel intLabel tok) (ST s) NT
    mkStart (NonTerminal l wp r) = do
      nt <- buildWrappedProd wp
      startNt <- freshNt NtNormal
      addProd $ NodeRule startNt l nt r
      return nt

    buildProd :: Prod' intLabel tok 'Interior
              -> ReaderT (MkGrammarState s nodeLabel intLabel tok) (ST s) (Seq (Sym intLabel NT tok))
    buildProd (Sequence ps) = mapM buildProd ps <&> fold
    buildProd (Terminal mLabels tk) =
      let unsnoc Seq.Empty = Nothing
          unsnoc (xs :|> x) = Just (xs, x)
          juggledLabels = mLabels >>= (toList >>> Seq.fromList >>> unsnoc)
      in Sym tk (juggledLabels <&> snd) (juggledLabels <&> fst & fold)
         & Seq.singleton & return
    buildProd (NonTerminalWrap labels (NonTerminal l wp r)) = buildWrappedProd wp
      <&> (\nt -> Nt nt (toList labels & Seq.fromList))
      <&> Seq.singleton
      <&> (fmap (\t -> Sym t Nothing mempty) l <>)
      <&> (<> fmap (\t -> Sym t Nothing mempty) r)
    buildProd (Alts as) = do
      nt <- freshNt NtNormal
      forM_ as $ \a -> do
        syms <- buildProd a
        addProd $ Rule nt syms
      return $ Seq.singleton $ Nt nt mempty
    buildProd (Many labels p) = do
      nt1 <- freshNt NtNormal
      nt2 <- freshNt NtNormal
      addProd $ Rule nt1 mempty
      addProd $ Rule nt1 [Nt nt2 mempty, Nt nt1 mempty]
      syms <- buildProd p
      addProd $ Rule nt2 syms
      return $ Seq.singleton $ Nt nt1 $ Seq.fromList labels

    buildWrappedProd :: WrappedProd intLabel tok -> ReaderT (MkGrammarState s nodeLabel intLabel tok) (ST s) NT
    buildWrappedProd (WrappedProd nt prod) = do
      MkGrammarState{alreadyStarted} <- ask
      started <- lift $ readSTRef alreadyStarted <&> S.member nt
      if started then return nt else do
        lift $ modifySTRef' alreadyStarted $ S.insert nt
        syms <- buildProd prod
        addProd $ Rule nt syms
        return nt
    buildWrappedProd (WrappedProds nt prods) = do
      MkGrammarState{alreadyStarted} <- ask
      started <- lift $ readSTRef alreadyStarted <&> S.member nt
      if started then return nt else do
        lift $ modifySTRef' alreadyStarted $ S.insert nt
        forM_ prods $ \(NonTerminal l wp r) -> do
          nt' <- buildWrappedProd wp
          addProd $ NodeRule nt l nt' r
        return nt

data NullStatus = Nulling | NonNullable deriving (Eq, Show)
data EpsNT = EpsNT !NT !NullStatus deriving (Eq, Show)

-- | Create a grammar in nihilistic normal form, from Aycock & Horspool 2002. Ensures that
-- each non-terminal is either strictly nulling or non-nullable, never proper nullable,
-- without changing the grammar too dramatically; the same non-terminals are used, but with
-- an added subscript to show if it's the nulling or non-nullable version, and the overall
-- structure is otherwise the same.
--
-- Because of the formulation of 'ParseTree' we will never need any nulling symbol, thus we
-- also remove all such symbols, and every rule with a nulling lefthand side.
--
-- The result is then a tuple containing: the start symbol if it is productive, a 'Bool'
-- indicatining if the grammar is nullable, all non-nullable rules with nulling symbols
-- removed from their righthand sides, and the set of top level nullable nodes.
mkNNFGrammar :: (Eq nodeLabel, Hashable nodeLabel)
             => (NT, Seq (Rule NT tok (Sym intLabel NT tok)), Seq (NtKind nodeLabel))
             -> (Maybe NT, Bool, Seq (Rule NT tok (Sym intLabel NT tok)), HashSet nodeLabel, Seq (NtKind nodeLabel))
mkNNFGrammar (startNt, prods, ntKinds) = (start, startNullable, prods', findTops startNt, ntKinds)
  where
    prods' = prods >>= expandProd
    startNullable = startNt `S.member` nullable
    start = if startNt `S.member` productive then Just startNt else Nothing

    nullable = nullableSet Nullable prods
    isProductive (Nt (EpsNT nt NonNullable) ls) = Just $ Nt nt ls
    isProductive (Sym k ml ls) = Just $ Sym k ml ls
    isProductive _ = Nothing
    productive = nullableSet Productive prods

    prodMap = prods <&> (getNt &&& Seq.singleton)
      & toList
      & M.fromListWith (<>)
    getNt = ruleAsTuple >>> fst
    findTops nt@(NT idx) = case Seq.index ntKinds idx of
      NtNode label -> S.singleton label
      _ -> M.lookup nt prodMap & fold & foldMap getSingleEntry
    getSingleEntry (ruleAsTuple -> (_, Seq.Empty :|> Nt nt _)) = findTops nt
    getSingleEntry _ = mempty

    expandProd (Rule nt syms) = mapM expandSym syms
      <&> (toList >>> mapMaybe isProductive >>> Seq.fromList)
      >>= \case
        Seq.Empty -> Seq.empty
        syms' -> return $ Rule nt syms'
    expandProd (NodeRule nt l nt' r) = expandNt nt' >>= \case
      EpsNT _ Nulling
        | Seq.length l > 0 || Seq.length r > 0
          -> return $ Rule nt $ (\t -> Sym t Nothing mempty) <$> l <> r
      EpsNT _ Nulling -> Seq.empty
      EpsNT _ NonNullable -> return $ NodeRule nt l nt' r
    expandNt :: NT -> Seq EpsNT
    expandNt nt = nullNt <> prodNt
      where
        nullNt = if nt `S.member` nullable then [EpsNT nt Nulling] else Seq.empty
        prodNt = if nt `S.member` productive then [EpsNT nt NonNullable] else Seq.empty
    expandSym :: Sym intLabel NT t -> Seq (Sym intLabel EpsNT t)
    expandSym (Nt nt ls) = Nt <$> expandNt nt <*> pure ls
    expandSym (Sym k ml ls) = Seq.singleton $ Sym k ml ls

data Nullability = Nullable | Productive

-- TODO: OPTIMIZE: implement a linear algorithm for this, and/or use less pointer chasing, and/or calculate both versions at once
nullableSet :: Nullability -> Seq (Rule NT tok (Sym intLabel NT tok)) -> HashSet NT
nullableSet nullStatus = toList
  >>> fmap ruleAsTuple
  >>> filter (snd >>> maybeInteresting)
  >>> findInteresting S.empty
  where
    maybeInteresting = case nullStatus of
      Nullable -> all $ \case
        Nt _ _ -> True
        _ -> False
      Productive -> Seq.null >>> not
    isInteresting = case nullStatus of
      Nullable -> \set -> all $ \case
        Nt nt _ -> nt `S.member` set
        _ -> False
      Productive -> \set -> any $ \case
        Nt nt _ -> nt `S.member` set
        _ -> True

    findInteresting prev prods
      | prev == next = prev
      | otherwise = findInteresting next nextProds
      where
        (nullProds, nextProds) = partition (snd >>> isInteresting prev) prods
        next = nullProds <&> fst & S.fromList & S.union prev

-- | Find the right-recursive non-terminals in the given NNF grammar
rightRecursive :: Seq (Rule NT t (Sym intLabel NT t)) -> HashSet NT
rightRecursive = toList
  >>> mapMaybe rightNt
  >>> M.fromListWith S.union
  >>> closeTrans
  >>> M.mapMaybeWithKey isRecursive
  >>> S.fromMap
  where
    rightNt (Rule nt (_ :|> Nt nt' _)) = Just (nt, S.singleton nt')
    rightNt _ = Nothing
    isRecursive nt nts
      | nt `S.member` nts = Just ()
      | otherwise = Nothing
    closeTrans prev = M.map (\here -> prev `M.intersection` S.toMap here & fold & S.union here) prev
