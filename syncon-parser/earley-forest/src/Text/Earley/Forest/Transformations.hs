{-# LANGUAGE RankNTypes, KindSignatures, ScopedTypeVariables, TupleSections, NamedFieldPuns, OverloadedLists, DeriveGeneric, LambdaCase, StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}

module Text.Earley.Forest.Transformations
( mkGrammar
, mkNNFGrammar
, Rule(..)
, NullStatus(..)
, Sym(..)
, NT(..)
, NtKind(..)
, EpsNT(..)
) where

import Prelude

import GHC.Exts (Any)
import GHC.Generics (Generic)
import Unsafe.Coerce (unsafeCoerce)

import Control.Arrow ((>>>))
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask, asks)
import Control.Monad.ST (runST, ST)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (first)
import Data.Foldable (forM_, toList)
import Data.Function ((&))
import Data.Functor ((<&>), void)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.List (partition)
import Data.STRef (STRef, newSTRef, readSTRef, modifySTRef')
import Data.Sequence (Seq, (<|), (|>))
import qualified Data.HashSet as S
import qualified Data.Sequence as Seq

import Text.Earley.Forest.Grammar (Grammar(..), runGrammar, Prod(..), TokKind)

data NtKind = NtMerge | NtNode | NtNormal | NtRanged deriving (Show, Eq, Generic)
data NT = NT !Int !NtKind deriving (Show, Eq, Generic)
instance Hashable NtKind
instance Hashable NT

data Sym nt t = Sym !(TokKind t) | Nt !nt
  deriving (Generic)
deriving instance (Eq nt, Eq (TokKind t)) => Eq (Sym nt t)
deriving instance (Show nt, Show (TokKind t)) => Show (Sym nt t)
instance (Hashable nt, Hashable (TokKind t)) => Hashable (Sym nt t)

-- | 'Rule nt syms sem' is a production with lefthand side 'nt', righthand side
-- 'syms', and semantics 'sem'. The semantics are described as a function that takes
-- an argument per element in 'syms', in reverse order. E.g., for a production
-- `A -> a b c` the semantics should be a function `\c b a -> result`.
data Rule nt sym (nodeF :: * -> *) = Rule !nt !(Seq sym) !Any

-- | A production (or list of productions) with the given non-terminal. No other
-- production has that non-terminal on the lefthand side.
data WrappedProd t nodeF
  = WrappedProd NT (Prod' t nodeF (nodeF (WrappedProd t nodeF)))
  | WrappedProds NT [Prod' t nodeF (WrappedProd t nodeF)]
type Prod' t nodeF = Prod (WrappedProd t nodeF) t nodeF

data MkGrammarState s t nodeF = MkGrammarState
  { nextNt :: !(STRef s Int)
  , alreadyStarted :: !(STRef s (HashSet NT))
  , rules :: !(STRef s (Seq (Rule NT (Sym NT t) nodeF))) }

-- | Construct a standard context-free grammar from the given 'Grammar'. Note that
-- the non-teminals 'NT' contain additional information about post-processing that
-- should be done on the result(s) derived using the non-teminal: nothing, merging,
-- or supplying range information. This function ensures that the resulting 'Any'
-- can be 'unsafeCoerce'd to the correct type (`a`, `HashSet NT`, `Maybe (t, t) -> a`,
-- respectively.
mkGrammar :: forall (nodeF :: * -> *) t.
             (forall r. Grammar r t nodeF (Prod r t nodeF r))
          -> (NT, Seq (Rule NT (Sym NT t) nodeF))
mkGrammar unfixedGrammar = runST $ do
  initialState@MkGrammarState{rules} <- MkGrammarState <$> newSTRef 0 <*> newSTRef S.empty <*> newSTRef Seq.empty
  startNt <- runReaderT (runGrammar mkRule mkAmbig unfixedGrammar >>= buildProd >>= mkStart) initialState
  (startNt,) <$> readSTRef rules
  where
    freshNt :: NtKind -> ReaderT (MkGrammarState s t nodeF) (ST s) NT
    freshNt k = do
      ref <- asks nextNt
      prev <- lift $ readSTRef ref <* modifySTRef' ref (+1)
      return $ NT prev k

    addProd :: Rule NT (Sym NT t) nodeF -> ReaderT (MkGrammarState s t nodeF) (ST s) ()
    addProd rule = do
      ref <- asks rules
      void $ lift $ modifySTRef' ref $ (|> rule)

    mkRule :: forall s. Prod (WrappedProd t nodeF) t nodeF (nodeF (WrappedProd t nodeF))
           -> ReaderT (MkGrammarState s t nodeF) (ST s) (Prod' t nodeF (WrappedProd t nodeF))
    mkRule p = do
      nt <- freshNt NtNode
      return $ NonTerminal (WrappedProd nt p) $ pure id
    mkAmbig :: forall s. [Prod (WrappedProd t nodeF) t nodeF (WrappedProd t nodeF)]
            -> ReaderT (MkGrammarState s t nodeF) (ST s) (Prod' t nodeF (WrappedProd t nodeF))
    mkAmbig ps = do
      nt <- freshNt NtMerge
      return $ NonTerminal (WrappedProds nt ps) $ pure id

    mkStart :: (Seq (Sym NT t), Any) -> ReaderT (MkGrammarState s t nodeF) (ST s) NT
    mkStart (syms, semantic) = do
      nt <- freshNt NtNormal
      addProd $ Rule nt syms semantic
      return nt

    buildProd :: Prod' t nodeF a
              -> ReaderT (MkGrammarState s t nodeF) (ST s) (Seq (Sym NT t), Any)
    buildProd (Pure a) = return (Seq.empty, toAny a)
    buildProd (Terminal k cont) = buildProd cont <&> first (Sym k <|)
    buildProd (NonTerminal wp cont) = do
      wpnt <- buildWrappedProd wp
      buildProd cont <&> first (Nt wpnt <|)
    buildProd (Ranged r cont) = do
      nt <- freshNt NtRanged
      (syms, semantic) <- buildProd r
      addProd $ Rule nt syms semantic
      buildProd cont <&> first (Nt nt <|)
    buildProd (Alts ps cont) = do
      nt <- freshNt NtNormal
      forM_ ps $ \p -> do
        (syms, semantic) <- buildProd p
        addProd $ Rule nt syms semantic
      buildProd cont <&> first (Nt nt <|)
    buildProd (Many p cont) = do
      nt1 <- freshNt NtNormal
      nt2 <- freshNt NtNormal
      addProd $ Rule nt1 Seq.empty (toAny ([] :: forall a. [a]))
      addProd $ Rule nt1 [Nt nt2, Nt nt1] (toAny $ flip (:))
      (syms, semantic) <- buildProd p
      addProd $ Rule nt2 syms semantic
      buildProd cont <&> first (Nt nt1 <|)

    buildWrappedProd :: WrappedProd t nodeF -> ReaderT (MkGrammarState s t nodeF) (ST s) NT
    buildWrappedProd (WrappedProd nt prod) = do
      MkGrammarState{alreadyStarted} <- ask
      started <- lift $ readSTRef alreadyStarted <&> S.member nt
      if started then return nt else do
        lift $ modifySTRef' alreadyStarted $ S.insert nt
        (syms, semantic) <- buildProd prod
        addProd $ Rule nt syms semantic
        return nt
    buildWrappedProd (WrappedProds nt prods) = do
      MkGrammarState{alreadyStarted} <- ask
      started <- lift $ readSTRef alreadyStarted <&> S.member nt
      if started then return nt else do
        lift $ modifySTRef' alreadyStarted $ S.insert nt
        forM_ prods $ \prod -> do
          (syms, semantic) <- buildProd prod
          addProd $ Rule nt syms semantic
        return nt

data NullStatus = Nulling | NonNullable deriving (Eq, Show)
data EpsNT = EpsNT !NT !NullStatus deriving (Eq, Show)

-- | Create a grammar in nihilistic normal form, from Aycock & Horspool 2002. Ensures that
-- each non-terminal is either strictly nulling or non-nullable, never proper nullable,
-- without changing the grammar too dramatically; the same non-terimnals are used, but with
-- an added subscript to show if it's the nulling or non-nullable version, and the overall
-- structure is otherwise the same.
-- As a result, there may be two starting symbols in the new grammar: the nulling and non-nullable
-- versions of the original start symbol.
mkNNFGrammar :: (NT, Seq (Rule NT (Sym NT t) nodeF))
             -> (Maybe EpsNT, Maybe EpsNT, Seq (Rule EpsNT (Sym EpsNT t) nodeF))
mkNNFGrammar (startNt, prods) = (start1, start2, prods')
  where
    prods' = prods >>= expandProd
    start1 = if startNt `S.member` productive then Just $ EpsNT startNt NonNullable else Nothing
    start2 = if startNt `S.member` nullable then Just $ EpsNT startNt Nulling else Nothing

    nullable = nullableSet Nullable prods
    isNulling (Nt (EpsNT _ Nulling)) = True
    isNulling _ = False
    productive = nullableSet Productive prods

    expandProd (Rule nt syms sem) = mapM expandSym syms <&> \syms' ->
      if all isNulling syms'
      then Rule (EpsNT nt Nulling) syms' sem
      else Rule (EpsNT nt NonNullable) syms' sem
    expandSym :: Sym NT t -> Seq (Sym EpsNT t)
    expandSym (Nt nt) = nullSym <> prodSym
      where
        nullSym = if nt `S.member` nullable then [Nt $ EpsNT nt Nulling] else Seq.empty
        prodSym = if nt `S.member` productive then [Nt $ EpsNT nt NonNullable] else Seq.empty
    expandSym (Sym k) = Seq.singleton $ Sym k

data Nullability = Nullable | Productive

-- TODO: OPTIMIZE: implement a linear algorithm for this, and/or use less pointer chasing, and/or calculate both versions at once
nullableSet :: Nullability -> Seq (Rule NT (Sym NT t) nodeF) -> HashSet NT
nullableSet nullStatus = toList
  >>> fmap (\(Rule nt syms _) -> (nt, syms))
  >>> filter (snd >>> maybeInteresting)
  >>> findInteresting S.empty
  where
    maybeInteresting = case nullStatus of
      Nullable -> all $ \case
        Nt _ -> True
        _ -> False
      Productive -> Seq.null >>> not
    isInteresting = case nullStatus of
      Nullable -> \set -> all $ \case
        Nt nt -> nt `S.member` set
        _ -> False
      Productive -> \set -> any $ \case
        Nt nt -> nt `S.member` set
        _ -> True

    findInteresting prev prods
      | prev == next = prev
      | otherwise = findInteresting next nextProds
      where
        (nullProds, nextProds) = partition (snd >>> isInteresting prev) prods
        next = nullProds <&> fst & S.fromList & S.union prev

toAny :: a -> Any
toAny = unsafeCoerce
