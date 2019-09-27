{-# OPTIONS_GHC -Wno-unused-imports #-} -- TODO: remove
{-# LANGUAGE NamedFieldPuns, FlexibleContexts, ScopedTypeVariables, DeriveGeneric, RankNTypes, ViewPatterns, LambdaCase, GeneralizedNewtypeDeriving, TypeApplications, TupleSections, RecordWildCards #-}

module Text.Earley.Forest.Parser
( precompute
, Precomputed
, recognize
, mkDerivation
, Node
, Error(..)
) where

import Prelude hiding (pred)

import GHC.Generics (Generic)

import Codec.Serialise (Serialise)
import Control.Applicative ((<|>))
import Control.Arrow ((>>>), first)
import Control.DeepSeq (NFData)
import Control.Monad (unless, join, (>=>))
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, asks, ask)
import Data.Foldable (toList, fold, forM_, foldl', foldr')
import Data.Function ((&))
import Data.Functor ((<&>), void)
import Data.HashMap.Lazy (HashMap)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.Maybe (catMaybes, mapMaybe, isJust)
import Data.STRef (STRef, newSTRef)
import Data.Sequence (Seq((:|>)), (|>))
import Data.Traversable (forM)
import Data.Vector (Vector)
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import qualified Data.STRef as STRef
import qualified Data.Sequence as Seq
import qualified Data.Vector as Vector

import Text.Earley.Forest.Grammar (TokKind, Parseable, getKind)
import Text.Earley.Forest.ParseTree (ParseTree(..))
import Text.Earley.Forest.SplitEpsDFA (EpsDFA(..), mkDFA, renumberStates, completedNT, isCompleted, DotProd(..), dotToRule)
import Text.Earley.Forest.Transformations (NT(..), Rule(..), Sym(..), NtKind(..), rightRecursive, NtKind(..), ruleAsTuple)
import qualified Text.Earley.Forest.ParseTree as PT

-- | Several Earley items, in the form of an EpsDFA state and origin
data Eim = Eim
  !Int -- ^ EpsDFA state
  !Int -- ^ origin set, i.e., which input position is the left edge of this item
  deriving (Show, Eq, Generic)
instance Hashable Eim

data Lim = Lim
  !Int -- ^ The set index of the top Eim
  !Eim -- ^ Top Eim
  !NT -- ^ The nt to transition on from it
  deriving (Show)

data ActiveSet s = ActiveSet
  { allItems :: !(STRef s (HashSet Eim))
  , workList :: !(STRef s [Eim])
  , numItemsActive :: !(STRef s Int)
  , currPostDot :: !(STRef s (HashMap NT (HashSet Eim)))
  , currIdx :: !Int
  , currLinks :: !(STRef s (HashMap Eim Links))
  }

data InactiveSet = InactiveSet
  { links :: !(HashMap Eim Links)
  , oldAllItems :: !(HashSet Eim)
  , postDot :: !(HashMap NT (Either (Eim, Lim) (HashSet Eim)))
  } deriving (Show)

data ParseState s = ParseState
  { prevSets :: !(STRef s (Seq InactiveSet))
  , currSet :: !(STRef s (ActiveSet s))
  , nextSet :: !(STRef s (ActiveSet s))
  }

data Links = Links
  { scanPreds :: HashSet Int
  -- ^ The predecessor split-eps-dfa states (origin must be the same as the 'Eim' we're linking from)
  , reduceLinks :: HashMap (NT, Int) (HashSet Int, HashSet (Either Int (NT, Eim)))
  -- ^ This is a map from (the 'NT' of the rightmost child, the index where the rightmost child starts)
  -- to (set of predecessor split-eps-dfa states, Either (direct 'Eim' child with given split-eps-dfa state) ('NT' of indirect lim child, lim child))
  } deriving (Show)
instance Semigroup Links where
  Links sp1 rl1 <> Links sp2 rl2 = Links (sp1 <> sp2) (M.unionWith (<>) rl1 rl2)
instance Monoid Links where
  mempty = Links mempty mempty
  mappend = (<>)

-- | The type of 'reduceLinks' after Lim links have been replaced with chains of normal
-- reduce links. (rightmost symbol nt, predIdx) -> (pred states, child states)
type ReduceLinks = HashMap (NT, Int) (HashSet Int, HashSet Int)

scanLink :: Int -> Links
scanLink predS = Links (S.singleton predS) mempty

data Link = CauseLess { pred :: !(Int, Eim) }
          | WithCause { pred :: !(Int, Eim), causeNt :: !NT, cause :: !Eim }
          | LimCause { pred :: !(Int, Eim), causeNt :: !NT, limCause :: !(NT, Eim) }

linkToLinks :: Link -> Links
linkToLinks CauseLess{pred = (_, Eim predS _)} = Links (S.singleton predS) mempty
linkToLinks WithCause{pred = (predIdx, Eim predS _), causeNt, cause = Eim s _} =
  Links mempty $ M.singleton (causeNt, predIdx) (S.singleton predS, S.singleton $ Left s)
linkToLinks LimCause{pred = (predIdx, Eim predS _), causeNt, limCause} =
  Links mempty $ M.singleton (causeNt, predIdx) (S.singleton predS, S.singleton $ Right limCause)

type ParseM s a = ReaderT (ParseState s) (ST s) a

-- | A bunch of precomputed things that are used for recognition and parsing.
data Precomputed nodeLabel intLabel t = Precomputed
  { -- | The starting symbol, if the language recognizes any non-empty string
    startNt :: !(Maybe NT)
    -- | Does this language accept the empty string?
  , nullable :: !Bool
    -- | The precomputed split epsilon DFA.
  , nts :: !(Seq (NtKind nodeLabel))
    -- | The split epsilon DFA most remaining fields are calculated from
  , epsDFA :: !(EpsDFA Int (Either NT t))
    -- | The set of non-terminals completed in a given DFA state.
  , completedMap :: !(HashMap Int (HashSet NT))
    -- | The transitions from a given DFA state labelled with terminals.
  , scanMap :: !(HashMap Int (HashMap t Int))
    -- | Store the states that are valid for elison, and the 'NT' they complete.
  , limMap :: !(HashMap Int NT)
    -- | The non-terminals for which it is interesting to create lim nodes
  , limInteresting :: !(HashSet NT)
    -- | The transition (if any) from a given DFA state with an epsilon label.
  , epsMap :: !(HashMap Int Int)
    -- | The transitions from a given DFA state with a 'NT' label.
  , postMap :: !(HashMap Int (HashMap NT Int))
    -- | The labels of the top level nodes that are nullable. These are the *only* nodes
    -- that will ever be constructed with 'mempty' as the interior.
  , nullableTops :: !(HashSet nodeLabel)
    -- | The righthand side of the productions completed in a given state, grouped by
    -- lefthand side, and which rule it is.
  , completedProdsMap :: !(HashMap Int (HashMap NT (HashMap Int (Rule NT t (Sym intLabel NT t)))))
  } deriving (Generic)
instance (NFData t, NFData nodeLabel, NFData intLabel) => NFData (Precomputed nodeLabel intLabel t)
instance (Serialise t, Serialise intLabel, Serialise nodeLabel, Eq t, Hashable t, Eq nodeLabel, Hashable nodeLabel) => Serialise (Precomputed nodeLabel intLabel t)

precompute :: (Eq t, Hashable t)
           => (Maybe NT, Bool, Seq (Rule NT t (Sym intLabel NT t)), HashSet nodeLabel, Seq (NtKind nodeLabel))
           -> Precomputed nodeLabel intLabel t
precompute (startNt, nullable, rules, nullableTops, nts) = Precomputed{..}
  where
    (states, epsDFA@EpsDFA{transitions}) = mkDFA (startNt, nullable, rules) & renumberStates
    completedMap = states <&> toList <&> fmap (completedNT rules) <&> catMaybes <&> S.fromList
    completedProdsMap = states <&> toList <&> fmap (dotToRule rules) <&> fmap ruleToMap <&> M.fromListWith M.union
      where
        ruleToMap (idx, rule) = (ruleAsTuple rule & fst, M.singleton idx rule)
    scanMap = transitions <&> \inner -> M.fromList [ (tk, v) | (Just (Right tk), v) <- M.toList inner ]
    limMap = completedMap & M.mapMaybe single & (`M.difference` haveSuccessors)
      where
        single (S.toList -> [nt]) = Just nt
        single _ = Nothing
        haveSuccessors = M.filter (M.null >>> not) transitions
    limInteresting = rightRecursive rules
    epsMap = M.lookup Nothing `M.mapMaybe` transitions
    postMap = transitions <&> \inner -> M.fromList [ (nt, v) | (Just (Left nt), v) <- M.toList inner ]

-- TODO: make good error reporting
-- | Parse a token stream, building the corresponding Earley sets. The returned tuple is
-- (the input stream as a 'Vector', the Earley sets).
recognize :: forall t nodeLabel intLabel f. (Eq (TokKind t), Hashable (TokKind t), Foldable f, Parseable t)
          => Precomputed nodeLabel intLabel (TokKind t)
          -> f t -> (Vector t, HashSet Eim, Seq InactiveSet)
recognize Precomputed
  { completedMap
  , epsDFA = EpsDFA{initial}
  , postMap
  , limMap
  , limInteresting
  , epsMap
  , scanMap
  }
  (toList >>> Vector.fromList -> input) = runST $ do
  state <- ParseState
    <$> newSTRef mempty
    <*> (newActiveSet 0 >>= newSTRef)
    <*> (newActiveSet 1 >>= newSTRef)
  (`runReaderT` state) $ do
    addInitial
    ActiveSet{allItems} <- doWhileM parseNotOver $ do
      set@ActiveSet{currIdx, currPostDot} <- asks currSet >>= readSTRef
      next <- asks nextSet >>= readSTRef
      let nextSymbolM = input Vector.!? currIdx <&> getKind

      forEachWorkItem set $ \eim@(Eim s origin) -> do
        -- Scan
        forM_ nextSymbolM $ \nextSymbol ->
          forM_ (M.lookup s scanMap >>= M.lookup nextSymbol) $ \nextS -> do
            let eim' = Eim nextS origin
            addEimTo next eim'
            addLinkTo next eim' $ scanLink s
        -- Reduce
        let completed = M.lookup s completedMap & fold
            mkLink nt predEim = WithCause{pred = (origin, predEim), cause = eim, causeNt = nt }
        unless (origin == currIdx || S.null completed) $ do
          posts <- inactiveSet origin <&> postDot
          let completions = toList completed
                & mapMaybe (\nt -> M.lookup nt posts <&> (nt,))
          forM_ completions $ \case
            (nt, Right eims) -> forM_ eims $ mkLink nt >>> stepAndAddEim set
            (nt, Left (_, Lim predIdx eim' nt')) -> stepAndAddEim set LimCause{pred = (predIdx, eim'), limCause = (nt, eim), causeNt = nt'}
        -- Compute postdot map
        modifySTRef' currPostDot $ M.unionWith S.union $ computePostDot eim

      -- No items left in the worklist
      -- readSTRef numItemsActive >>= \num -> traceM $ "Items at " <> show currIdx <> ": " <> show num
      -- items <- readSTRef allItems
      -- forM_ items $ \item@(Eim s _) ->
      --   traceM $ "  " <> show item <> " -> " <> foldMap show (M.lookup s completedMap)
      stepSets
      return set

    -- Parse over
    items <- readSTRef allItems
    -- traceM $ "Total tokens: " <> show (Vector.length input)
    -- traceM $ "Start symbol: " <> show startNt
    -- prevs <- asks prevSets >>= readSTRef
    -- _ <- (`Seq.traverseWithIndex` prevs) $ \i InactiveSet{links, postDot} -> do
    --   traceM $ show i <> ":"
    --   forM_ (M.toList links) $ \(eim, ls) -> traceM $ "  " <> show eim <> " -> " <> show ls
    --   forM_ (M.toList postDot) $ \(nt, rhs) -> traceM $ "  " <> show nt <> " -> " <> show rhs
    prev <- asks prevSets >>= readSTRef
    return (input, items, prev)
  where
    newActiveSet :: Int -> ST s (ActiveSet s)
    newActiveSet currIdx = do
      allItems <- newSTRef mempty
      workList <- newSTRef mempty
      numItemsActive <- newSTRef 0
      currPostDot <- newSTRef mempty
      currLinks <- newSTRef mempty
      return ActiveSet{currIdx, allItems, workList, numItemsActive, currPostDot, currLinks}

    parseNotOver :: ParseM s Bool
    parseNotOver = asks currSet >>= readSTRef >>= \ActiveSet{allItems, currIdx} ->
      if currIdx > Vector.length input then return False else
        readSTRef allItems <&> not . S.null

    -- | Move the current active set to the sequence of old sets, set the next set as
    -- current, and create a new next set.
    stepSets :: ParseM s ()
    stepSets = do
      ParseState{prevSets, currSet, nextSet} <- ask
      readSTRef currSet >>= mkInactiveSet >>= \inactive -> modifySTRef' prevSets (|> inactive)
      next@ActiveSet{currIdx} <- readSTRef nextSet
      writeSTRef currSet next
      lift (newActiveSet $ currIdx+1) >>= writeSTRef nextSet

    addInitial :: ParseM s ()
    addInitial = asks currSet >>= readSTRef >>= (`addEimTo` Eim initial 0)

    -- | Perform the computations required to move an active set to the inactive sequence,
    -- most importantly, compute the postDot map, including 'Lim's.
    mkInactiveSet :: ActiveSet s -> ParseM s InactiveSet
    mkInactiveSet ActiveSet{allItems, currPostDot, currIdx, currLinks} = do
      items <- readSTRef allItems
      postDots <- readSTRef currPostDot
      links <- readSTRef currLinks
      M.traverseWithKey (go postDots) postDots
        <&> InactiveSet links items
      where
        go postDots nt prev@(S.toList -> [eim@(Eim s origin)])
          | not $ nt `S.member` limInteresting = return $ Right prev
          | otherwise = do  -- OPTIMIZE: this may recompute the same lims multiple times (when they are from the same earley set)
              mLim <- fmap join $ forM (M.lookup s postMap >>= M.lookup nt) $ \postS ->
                fmap join $ forM (M.lookup postS limMap) $ \nt' ->
                  if currIdx /= origin
                  then inactiveSet origin <&> postDot <&> M.lookup nt' <&> \case
                    Just (Left lim) -> Just lim
                    _ -> Nothing
                  else forM (M.lookup nt' postDots) (go postDots nt') <&> (>>= fromLeft)
              let res = (eim,) $ maybe (Lim currIdx eim nt) snd mLim
              return $ Left res
        go _ _ eims = return $ Right eims
        fromLeft (Left a) = Just a
        fromLeft _ = Nothing

    -- | Add an 'Eim' to a given 'ActiveSet'. Note that newly added 'Eim's will be processed
    -- by 'forEachWorkItem'.
    addEimTo :: ActiveSet s -> Eim -> ParseM s ()
    addEimTo ActiveSet{allItems, workList, currIdx, numItemsActive} eim@(Eim s _) =
      unlessM (readSTRef allItems <&> S.member eim) $ do
        modifySTRef' allItems $ S.insert eim  -- NOTE: add confirmed
        modifySTRef' workList (eim:)
        modifySTRef' numItemsActive (+1)
        forM_ (M.lookup s epsMap) $ \next -> do
          let eim' = Eim next currIdx
          unlessM (readSTRef allItems <&> S.member eim') $ do
            modifySTRef' allItems $ S.insert eim'  -- NOTE: add predicted
            modifySTRef' workList (eim':)
            modifySTRef' numItemsActive (+1)

    -- | Step over the given 'NT', then add the resulting 'Eim' to the given 'ActiveSet'
    stepAndAddEim :: ActiveSet s -> Link -> ParseM s ()
    stepAndAddEim as link = forM_ (M.lookup s postMap >>= M.lookup nt) $ \postS -> do
      let eim = Eim postS origin
      addEimTo as eim
      addLinkTo as eim $ linkToLinks link
      where
        Eim s origin = pred link & snd
        nt = causeNt link

    addLinkTo :: ActiveSet s -> Eim -> Links -> ParseM s ()
    addLinkTo ActiveSet{currLinks} eim links =
      modifySTRef' currLinks $ M.insertWith (<>) eim links

    -- | Perform an action for every 'Eim' in the given 'ActiveSet'. Note that adding new
    -- 'Eim's to the set inside the action is supported (and intended), those items will
    -- also be processed. This implies that the worklist for the set is empty when
    -- 'forEachWorkItem' returns
    forEachWorkItem :: ActiveSet s -> (Eim -> ParseM s ()) -> ParseM s ()
    forEachWorkItem set@ActiveSet{workList} op = readSTRef workList >>= \case
      [] -> return ()
      eim : rest -> writeSTRef workList rest >> op eim >> forEachWorkItem set op

    -- | Fetch a previous set. Crashes if the set at the given index isn't inactive yet.
    inactiveSet :: Int -> ParseM s InactiveSet
    inactiveSet idx = asks prevSets >>= readSTRef <&> (`Seq.index` idx)

    -- | Find the 'NT's after a dot in any dotted production in the state in the 'Eim'.
    computePostDot :: Eim -> HashMap NT (HashSet Eim)
    computePostDot eim@(Eim s _) = M.lookup s postMap & fold <&> const (S.singleton eim)

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM cond m = cond >>= \b -> if b then return () else m

doWhileM :: Monad m => m Bool -> m a -> m a
doWhileM cond m = m >>= \res -> cond >>= \b -> if b then doWhileM cond m else return res

readSTRef :: STRef s a -> ReaderT r (ST s) a
readSTRef = STRef.readSTRef >>> lift

modifySTRef' :: STRef s a -> (a -> a) -> ReaderT r (ST s) ()
modifySTRef' ref = STRef.modifySTRef' ref >>> lift

writeSTRef :: STRef s a -> a -> ReaderT r (ST s) ()
writeSTRef ref = STRef.writeSTRef ref >>> lift

newtype Node = Node Int deriving (Eq, Hashable, Show, NFData, Serialise)

-- | Expected next tokens. 'Nothing' corresponds to EOF.
data Error nodeLabel tok
  = Error (HashSet (Maybe (TokKind tok))) (Maybe tok)

type Result nodeF = Either (HashSet Node) (InteriorResult nodeF)
type InteriorResult nodeF = Seq (InteriorF nodeF (HashSet Node))

data DerivationState s nodeF = DerivationState
  { nextNode :: !(STRef s Int)
  , alreadyDone :: !(STRef s (HashMap (NT, Int, Int) (Result nodeF)))
  , nodes :: !(STRef s (HashMap Node (nodeF (HashSet Node))))
  }

type DerivM s nodeF a = ReaderT (DerivationState s nodeF) (ST s) a

data SemanticProd nodeF
  = NormalProd !(Seq (Sym (IntLabel nodeF) NT (TokKind (Tok nodeF)))) !(Seq (InteriorF nodeF (HashSet Node)))
  | PreNodeProd !(Seq (TokKind (Tok nodeF))) !NT !(Seq (TokKind (Tok nodeF)))
  | PostNodeProd !(Seq (TokKind (Tok nodeF))) !(HashSet Node)

mkDerivation :: forall nodeF. (ParseTree nodeF, Parseable (Tok nodeF), Eq (TokKind (Tok nodeF)), Hashable (TokKind (Tok nodeF)))
             => Precomputed (NodeLabel nodeF) (IntLabel nodeF) (TokKind (Tok nodeF))
             -> (Vector (Tok nodeF), HashSet Eim, Seq InactiveSet)
             -> Either (Error (NodeLabel nodeF) (Tok nodeF)) (HashMap Node (nodeF (HashSet Node)), HashSet Node)
mkDerivation Precomputed{nullable = True, nullableTops} (input, _, _)
  | Vector.length input == 0 = Right (topNodes, S.fromMap $ void topNodes)
    where
      topNodes = node <$> toList nullableTops <*> pure Nothing <*> pure (emptyInt @nodeF)
        & zip (Node <$> [0..])
        & M.fromList
mkDerivation pc@Precomputed
  { startNt = Just startNt
  , postMap
  , completedMap
  , completedProdsMap
  , nts
  }
  (input, finalItems, oldSets)
  | Seq.length oldSets == Vector.length input + 1
  , let finalSs = finalItems & S.filter completesStartNt & S.map (\(Eim s _) -> s)
  , not $ S.null finalSs = runST $ do
    state@DerivationState{nodes} <- DerivationState <$> newSTRef 0 <*> newSTRef mempty <*> newSTRef mempty
    (`runReaderT` state) $
      derivEims mempty startNt (0, Seq.length oldSets - 1) finalSs >>= \case
        Left tops -> readSTRef nodes <&> (, tops) <&> Right
        Right _ -> error "The top non-terminal somehow did not produce a node, but rather an interior"
  | otherwise = Left $ mkError pc input oldSets
  where
    completesStartNt :: Eim -> Bool
    completesStartNt (Eim s _) = M.lookup s completedMap & fold & S.member startNt

    newSemanticProd :: Rule NT (TokKind (Tok nodeF)) (Sym (IntLabel nodeF) NT (TokKind (Tok nodeF))) -> SemanticProd nodeF
    newSemanticProd (Rule _ syms) = NormalProd syms (Seq.singleton (emptyInt @nodeF))
    newSemanticProd (NodeRule _ l nt r) = PreNodeProd l nt r

    endsWith :: Either (NT, Result nodeF) (Tok nodeF) -> SemanticProd nodeF -> Maybe (SemanticProd nodeF)
    endsWith (Left (nt, res)) (NormalProd (rest :|> Nt nt' ntLabels) interior)
      | nt' == nt = case (res, ntLabels) of
          (Left nodes, labels :|> firstLabel) ->
            let interiors' = applyLabels labels $ Seq.singleton $ nodeLeaf @nodeF firstLabel nodes
            in cattree <$> interiors' <*> interior
               & NormalProd rest
               & Just
          (Right interiors, labels) ->
            let interiors' = foldr' (\l ints -> label @nodeF l <$> ints) interiors labels
            in cattree <$> interiors' <*> interior
               & NormalProd rest
               & Just
          _ -> Nothing -- TODO: maybe crash here? this should never happen
      where
        applyLabels labels interiors = foldr' (\l ints -> label @nodeF l <$> ints) interiors labels
        cattree = (<->) @nodeF
    endsWith (Right tok) (NormalProd (rest :|> Sym tk mLabel labels) interior)
      | tk == getKind tok = case mLabel of
          Nothing -> Just $ NormalProd rest interior
          Just l -> foldr' (label @nodeF) (token @nodeF l tok) labels
            & (\int -> (<->) @nodeF int <$> interior)
            & NormalProd rest
            & Just
    endsWith (Left (nt, res)) (PreNodeProd l nt' Seq.Empty)
      | nt == nt' = case res of
          Left res' -> PostNodeProd l res' & Just
          Right _ -> Nothing -- TODO: maybe crash here? this should never happen
    endsWith (Right tok) (PreNodeProd l nt' (r :|> tk))
      | getKind tok == tk = PreNodeProd l nt' r & Just
    endsWith (Right tok) (PostNodeProd (l :|> tk) res)
      | getKind tok == tk = PostNodeProd l res & Just
    endsWith _ _ = Nothing

    -- TODO: need to properly handle when there's an NT with NtMerge, as well as what to do with
    -- parenthesized expressions, i.e., passthrough productions

    finishSemantics :: Int -> Int -> NT -> Result nodeF -> DerivM s nodeF (Result nodeF)
    finishSemantics originIdx setIdx (NT ntIdx) sems = case (Seq.index nts ntIdx, sems) of
      (NtNode nodeLabel, Right sems') -> mapM (mkNode nodeLabel) sems'
        <&> foldMap S.singleton
        <&> Left
      _ -> return sems
      where
        location
          | originIdx < setIdx = Just (input Vector.! originIdx, input Vector.! (setIdx - 1))
          | otherwise = Nothing
        mkNode nodeLabel interior = do
          DerivationState{nextNode, nodes} <- ask
          nodeId <- readSTRef nextNode <* modifySTRef' nextNode (+1)
          modifySTRef' nodes $ M.insert (Node nodeId) $ node nodeLabel location interior
          return $ Node nodeId

    -- | Produce the semantic result for the given 'NT', covering the given range, knowing
    -- that the 'Eim's from which it is to be derived have exactly the states given in the
    -- third argument
    derivEims :: HashMap Eim ReduceLinks -> NT -> (Int, Int) -> HashSet Int -> DerivM s nodeF (Result nodeF)
    derivEims extraLinks nt (originIdx, setIdx) ss = do
      DerivationState{alreadyDone} <- ask
      readSTRef alreadyDone <&> M.lookup (nt, originIdx, setIdx) >>= \case
        Just res -> return res
        Nothing -> do
          let eims = S.map (\s -> Eim s originIdx) ss
              -- | The 'SemanticProd's that are completed in the active states
              completed :: Seq (SemanticProd nodeF)
              completed = foldMap ((`M.lookup` completedProdsMap) >=> M.lookup nt) ss
                & fold
                & toList
                & Seq.fromList
                <&> newSemanticProd
              -- | The original links, including Lim links
              Links scanPredSs allReduceCauses = Seq.index oldSets setIdx
                & links
                & (`M.intersection` S.toMap eims)
                & fold
          extraRef <- lift $ newSTRef extraLinks
          -- Go through the reduction causes, replace Lim links with normal links, and store the
          -- intermediate links in 'extraRef'
          realReduceCauses <- (`M.traverseWithKey` allReduceCauses) $ \(nt', predIdx) (predSs, children) -> do
            children' <- forM (S.toList children) $ \case
              Left s -> return s
              Right lim -> do
                let (Eim s _, links) = limChain nt' predIdx lim
                modifySTRef' extraRef $ M.unionWith (M.unionWith (<>)) links
                return s
            return (predSs, S.fromList children')
          finalExtras <- readSTRef extraRef
          -- Combine the different sources of reduction links
          let reduceCauses = finalExtras `M.intersection` S.toMap eims
                & foldl' (M.unionWith (<>)) mempty
                & M.unionWith (<>) realReduceCauses
          -- Compute the results produced by scanning
          scanRes <- if S.null scanPredSs then return $ Right mempty else do
            let tok = input Vector.! (setIdx-1)
            toList completed
              & mapMaybe (endsWith $ Right tok)
              & Seq.fromList
              & contDeriv (originIdx, setIdx-1) scanPredSs
          -- Compute the results produced by stepping over a non-terminal, and inserting the
          -- result of that non-terminal as a child
          reduceRes <- forM (M.toList reduceCauses) $ \((nt', predIdx), (predSs, childSs)) -> do
            children <- derivEims finalExtras nt' (predIdx, setIdx) childSs
            toList completed
              & mapMaybe (endsWith $ Left (nt', children))
              & Seq.fromList
              & contDeriv (originIdx, predIdx) predSs
          -- Wrap everything up by performing the final action dictated by the non-terminal
          res <- scanRes : reduceRes
            & foldl' keepLeft (Right mempty)
            & finishSemantics originIdx setIdx nt
          modifySTRef' alreadyDone $ M.insert (nt, originIdx, setIdx) res
          return res

    -- | limChain nt originIdx (botNt, botEim)
    -- Perform the (deterministic) sequence of reductions, starting with the
    -- completed 'Eim' `botEim` (that completes the non-terminal `botNt`) until
    -- we produce an 'Eim' of the form `Eim s originIdx` that completes the
    -- non-terminal `nt`. Return that topmost 'Eim' and the reduction links produced
    -- along the way.
    limChain :: NT -> Int -> (NT, Eim) -> (Eim, HashMap Eim ReduceLinks)
    limChain topNt originIdx = go mempty
      where
        go links (nt, eim@(Eim _ originIdx'))
          | topNt == nt
          , originIdx == originIdx' = (eim, links)
        go links (nt, Eim s origin) =
          let Eim parentS parentOrigin = Seq.index oldSets origin
                & postDot
                & M.lookup nt
                & fromJust
                & either fst mkSingle
              nextS = M.lookup parentS postMap >>= M.lookup nt & fromJust
              eim = Eim nextS parentOrigin
              eimNt = M.lookup nextS completedMap & fold & mkSingle
              addNewLink = M.insertWith (M.unionWith (<>)) eim $
                M.singleton (nt, origin) (S.singleton parentS, S.singleton s)
          in go (addNewLink links) (eimNt, eim)
        fromJust (Just a) = a
        fromJust _ = error "fromJust failed in limChain"
        mkSingle (S.toList -> [a]) = a
        mkSingle _ = error "Got more than one result when trying to construct a Lim chain"

    contDeriv :: (Int, Int) -> HashSet Int -> Seq (SemanticProd nodeF) -> DerivM s nodeF (Result nodeF)
    contDeriv (originIdx, setId) _ semProds
      | originIdx == setId = semProds <&> getSem & foldl' keepLeft (Right mempty) & return
      where
        getSem (NormalProd _ sem) = Right sem
        getSem (PreNodeProd _ _ _) = Right mempty
        getSem (PostNodeProd _ sem) = Left sem
    contDeriv (originIdx, setIdx) ss semProds = do
      let eims = S.map (\s -> Eim s originIdx) ss
          Links scanPredSs allReduceCauses = Seq.index oldSets setIdx
            & links
            & (`M.intersection` S.toMap eims)
            & fold
      extraRef <- lift $ newSTRef mempty
      -- Go through the reduction causes, replace Lim links with normal links, and store the
      -- intermediate links in 'extraRef'
      realReduceCauses <- (`M.traverseWithKey` allReduceCauses) $ \(nt', predIdx) (predSs, children) -> do
        children' <- forM (S.toList children) $ \case
          Left s -> return s
          Right lim -> do
            let (Eim s _, links) = limChain nt' predIdx lim
            modifySTRef' extraRef $ M.unionWith (M.unionWith (<>)) links
            return s
        return (predSs, S.fromList children')
      finalExtras <- readSTRef extraRef
      -- Combine the different sources of reduction links
      let reduceCauses = finalExtras `M.intersection` S.toMap eims
            & foldl' (M.unionWith (<>)) mempty
            & M.unionWith (<>) realReduceCauses
      -- Compute the results produced by scanning
      scanRes <- if S.null scanPredSs then return $ Right mempty else do
        let tok = input Vector.! (setIdx-1)
        toList semProds
          & mapMaybe (endsWith $ Right tok)
          & Seq.fromList
          & contDeriv (originIdx, setIdx-1) scanPredSs
      -- Compute the results produced by stepping over a non-terminal, and inserting the
      -- result of that non-terminal as a child
      reduceRes <- forM (M.toList reduceCauses) $ \((nt', predIdx), (predSs, childSs)) -> do
        children <- derivEims finalExtras nt' (predIdx, setIdx) childSs
        toList semProds
          & mapMaybe (endsWith $ Left (nt', children))
          & Seq.fromList
          & contDeriv (originIdx, predIdx) predSs
      scanRes : reduceRes
        & foldl' keepLeft (Right mempty)
        & return
mkDerivation pc (input, _, oldSets) = Left $ mkError pc input oldSets

mkError :: (Eq (TokKind t), Hashable (TokKind t))
        => Precomputed nodeLabel intLabel (TokKind t)
        -> Vector t -> Seq InactiveSet -> Error nodeLabel t
mkError Precomputed{scanMap} input sets = Error expected found
  where
    found = input Vector.!? Seq.length sets
    expected = Seq.index sets (Seq.length sets - 1)
      & oldAllItems
      & S.map (\(Eim s _) -> s)
      & toList
      <&> (`M.lookup` scanMap)
      & foldMap fold
      & void
      & S.fromMap
      & S.map Just

keepLeft :: (Monoid l, Monoid r) => Either l r -> Either l r -> Either l r
keepLeft (Left a) (Left b) = Left $ a <> b
keepLeft a@Left{} _ = a
keepLeft _ a@Left{} = a
keepLeft (Right a) (Right b) = Right $ a <> b
