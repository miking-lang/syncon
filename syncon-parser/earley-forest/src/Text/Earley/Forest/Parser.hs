{-# LANGUAGE NamedFieldPuns, FlexibleContexts, ScopedTypeVariables, DeriveGeneric, RankNTypes, ViewPatterns, LambdaCase, GeneralizedNewtypeDeriving, TypeApplications, TupleSections #-}

module Text.Earley.Forest.Parser (parse, Node) where

import Prelude hiding (pred)

import GHC.Generics (Generic)
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

import Data.STRef (STRef, newSTRef)
import qualified Data.Sequence as Seq
import qualified Data.STRef as STRef
import Control.DeepSeq (NFData)
import Control.Monad (unless, join, (>=>))
import Control.Arrow ((>>>), first)
import Data.Functor ((<&>), void)
import Data.Sequence (Seq((:|>)), (|>))
import Data.Function ((&))
import Data.Traversable (forM)
import Data.Hashable (Hashable)
import Data.Vector (Vector)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (toList, fold, forM_, foldl')
import Control.Applicative ((<|>))
import Data.Maybe (catMaybes, mapMaybe, isJust)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, asks, ask)
import Data.HashMap.Lazy (HashMap)
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import qualified Data.HashMap.Lazy as M
import qualified Data.Vector as Vector

import Text.Earley.Forest.Grammar (TokKind, Parseable, getKind)
import Text.Earley.Forest.Transformations (EpsNT(..), NT(..), Rule(..), Sym(..), NullStatus(..), NtKind(..))
import Text.Earley.Forest.SplitEpsDFA (EpsDFA(..), mkDFA, renumberStates, completedNT, isCompleted, DotProd(..))

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

-- TODO: make good error reporting
parse :: forall t nodeF f. (Eq (TokKind t), Hashable (TokKind t), Foldable f, Parseable t)
      => (Maybe EpsNT, Maybe EpsNT, Seq (Rule EpsNT (Sym EpsNT t) nodeF))
      -> f t -> Either () (HashMap Node (nodeF (HashSet Node)), HashSet Node)
parse nnf@(s1, s2, rules) = case s1 <|> s2 of
  Nothing -> const $ Left ()  -- NOTE: no start symbol, so the parser always fails
  Just (EpsNT nt _) -> doParse nt
  where
    dfa@(states, EpsDFA{initial, transitions}) = mkDFA nnf & renumberStates

    -- | The set of non-terminals completed in a given DFA state.
    completedMap :: HashMap Int (HashSet NT)
    completedMap = states <&> toList <&> fmap (completedNT rules) <&> catMaybes <&> S.fromList

    -- | The transitions from a given DFA state labelled with terminals.
    scanMap :: HashMap Int (HashMap (TokKind t) Int)
    scanMap = transitions <&> \inner -> M.fromList [ (tk, v) | (Just (Sym tk), v) <- M.toList inner ]

    -- | Store the states that are valid for elison, and the 'NT' they complete.
    limMap :: HashMap Int NT
    limMap = completedMap & M.mapMaybe single & (`M.difference` haveSuccessors)
      where
        single (S.toList -> [nt]) = Just nt
        single _ = Nothing
        haveSuccessors = M.filter (M.null >>> not) transitions

    -- | The transition (if any) from a given DFA state with an epsilon label.
    epsMap :: HashMap Int Int
    epsMap = M.lookup Nothing `M.mapMaybe` transitions

    -- | The transitions from a given DFA state with a 'NT' label.
    postMap :: HashMap Int (HashMap NT Int)
    postMap = transitions <&> \inner -> M.fromList [ (nt, v) | (Just (Nt nt), v) <- M.toList inner ]

    -- | Find the 'NT's after a dot in any dotted production in the state in the 'Eim'.
    computePostDot :: Eim -> HashMap NT (HashSet Eim)
    computePostDot eim@(Eim s _) = M.lookup s postMap & fold <&> const (S.singleton eim)

    mkDerivations :: NT -> Vector t -> [Eim] -> Seq InactiveSet -> (HashMap Node (nodeF (HashSet Node)), HashSet Node)
    mkDerivations nt = constructDerivations nt rules dfa postMap

    -- | Actually do the parse. Everything above is constant and does not depend on the input, and can
    -- thus be precomputed.
    doParse :: NT -> f t -> Either () (HashMap Node (nodeF (HashSet Node)), HashSet Node)
    doParse startNt (toList >>> Vector.fromList -> input) = runST $ do
      state <- ParseState
        <$> newSTRef mempty
        <*> (newActiveSet 0 >>= newSTRef)
        <*> (newActiveSet 1 >>= newSTRef)
      (`runReaderT` state) $ do
        addInitial
        ActiveSet{currIdx, allItems} <- doWhileM parseNotOver $ do
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
                (nt, Right eims) -> forM_ eims $ mkLink nt  >>> stepAndAddEim set
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
        let completingEims = filter completesStartNt $ toList items
        if currIdx == Vector.length input && not (null completingEims)
          then asks prevSets >>= readSTRef <&> mkDerivations startNt input completingEims <&> Right
          else return $ Left ()
      where
        completesStartNt :: Eim -> Bool
        completesStartNt (Eim s 0) = M.lookup s completedMap & fold & S.member startNt
        completesStartNt _ = False

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
        mkInactiveSet ActiveSet{currPostDot, currIdx, currLinks} = do
          postDots <- readSTRef currPostDot <&> fmap splitSingles
          links <- readSTRef currLinks
          M.traverseWithKey (go postDots) postDots
            <&> InactiveSet links
          where
            splitSingles (S.toList -> [eim]) = Left eim
            splitSingles eims = Right eims
            go postDots nt (Left eim@(Eim s origin)) = do  -- OPTIMIZE: this may recompute the same lims multiple times (when they are from the same earley set)
              mLim <- fmap join $ forM (M.lookup s postMap >>= M.lookup nt) $ \postS ->
                fmap join $ forM (M.lookup postS limMap) $ \nt' ->
                  if currIdx /= origin
                  then inactiveSet origin <&> postDot <&> M.lookup nt' <&> \case
                    Just (Left lim) -> Just lim
                    _ -> Nothing
                  else forM (M.lookup nt' postDots) (go postDots nt') <&> (>>= fromLeft)
              return $ Left $ (eim,) $ maybe (Lim currIdx eim nt) snd mLim
            go _ _ (Right eims) = return $ Right eims
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

-- Negative indices for epsilon derivations, non-negative for other derivations
newtype Node = Node Int deriving (Eq, Hashable, Show, NFData)

data NullDerivationState s t nodeF = NullDerivationState
  { n_nextNode :: !(STRef s Int)
  , n_alreadyDone :: !(STRef s (HashMap NT (Seq Any)))
  , nullRules :: !(HashMap NT (Seq (Seq (Sym EpsNT t), Any)))
  , n_nodes :: !(STRef s (HashMap Node (nodeF (HashSet Node))))
  }

type NullDerivM s t nodeF a = ReaderT (NullDerivationState s t nodeF) (ST s) a

data DerivationState s t nodeF = DerivationState
  { nextNode :: !(STRef s Int)
  , alreadyDone :: !(STRef s (HashMap (NT, Int, Int) (Seq Any)))
  , nodes :: !(STRef s (HashMap Node (nodeF (HashSet Node))))
  }

type DerivM s t nodeF a = ReaderT (DerivationState s t nodeF) (ST s) a

type SemanticProd t = (Seq (Sym EpsNT t), Seq Any)

constructDerivations :: forall t nodeF. (Eq (TokKind t), Parseable t)
                     => NT
                     -> Seq (Rule EpsNT (Sym EpsNT t) nodeF)
                     -> (HashMap Int (HashSet DotProd), EpsDFA Int (Sym NT t))
                     -> HashMap Int (HashMap NT Int)
                     -> Vector t
                     -> [Eim] -> Seq InactiveSet
                     -> (HashMap Node (nodeF (HashSet Node)), HashSet Node)
constructDerivations startNt rules (states, _) postMap = doConstruct
  where
    -- | The parts of the derivations that stem from nulling non-terminals are constant,
    -- they do not depend on any information from any parse, and can thus be precomputed.
    epsMap :: HashMap NT (Seq Any)
    epsNodes :: HashMap Node (nodeF (HashSet Node))
    (epsMap, epsNodes) = runST $ do
      let nullRules = [ (nt, Seq.singleton (syms, sem))
                      | Rule (EpsNT nt Nulling) syms sem <- toList rules]
                      & M.fromListWith (<>)
      state@NullDerivationState{n_alreadyDone, n_nodes} <- NullDerivationState <$> newSTRef (-1) <*> newSTRef mempty <*> pure nullRules <*> newSTRef mempty
      (`runReaderT` state) $ do
        mapM_ requestEpsNt $ M.keys nullRules
        (,) <$> readSTRef n_alreadyDone <*> readSTRef n_nodes

    -- | Get the result of constructing a parse forest for the given nulling non-terminal. If
    -- the result hasn't already been computed, do so first.
    requestEpsNt :: NT -> NullDerivM s t nodeF (Seq Any)
    requestEpsNt nt = do
      NullDerivationState{n_alreadyDone, nullRules} <- ask
      readSTRef n_alreadyDone <&> M.lookup nt >>= \case
        Just res -> return res
        Nothing -> do
          res <- M.lookup nt nullRules & fold & constructEpsNt nt
          modifySTRef' n_alreadyDone $ M.insert nt res
          return res

    -- | Construct the derivations for a given nulling non-terminal, with the given alternatives.
    constructEpsNt :: NT -> Seq (Seq (Sym EpsNT t), Any) -> NullDerivM s t nodeF (Seq Any)
    constructEpsNt (NT _ kind) alts = do
      results <- mapM (\(syms, sem) -> constructEpsSeq (Seq.singleton sem) syms) alts
        <&> join
      case kind of
        NtNormal -> return results
        NtRanged -> return $ anyToRangeFunc <$> results <*> pure Nothing
        NtNode -> results <&> anyToNodeF & mapM mkEpsNode <&> foldMap S.singleton
          <&> nodesToAny <&> Seq.singleton
        NtMerge -> foldMap anyToNodes results & nodesToAny & Seq.singleton & return

    -- | Construct the derivations for a sequence of symbols, using the given possible semantics.
    constructEpsSeq :: Seq Any -> Seq (Sym EpsNT t) -> NullDerivM s t nodeF (Seq Any)
    constructEpsSeq sems Seq.Empty = return sems
    constructEpsSeq _ (_ Seq.:|> Sym _) = error "Found a terminal in a nulling production"
    constructEpsSeq sems (rest Seq.:|> Nt (EpsNT nt _)) = do
      res <- requestEpsNt nt
      let nextSems = anyToFunc <$> sems <*> res
      constructEpsSeq nextSems rest

    -- | Construct a new node in the DAG for an epsilon node.
    mkEpsNode :: nodeF (HashSet Node) -> NullDerivM s t nodeF Node
    mkEpsNode node = do
      NullDerivationState{n_nextNode, n_nodes} <- ask
      nodeId <- readSTRef n_nextNode <* modifySTRef' n_nextNode (+ (-1))
      modifySTRef' n_nodes $ M.insert (Node nodeId) node
      return $ Node nodeId

    -- | These functions are for converting between the various possible semantics. Note that
    -- we use these more strictly typed variations instead of using 'unsafeCoerce' directly,
    -- to lessen the risk of messing up.
    anyToFunc :: Any -> (Any -> Any)
    anyToFunc = unsafeCoerce

    anyToRangeFunc :: Any -> (Maybe (t, t) -> Any)
    anyToRangeFunc = unsafeCoerce

    anyToNodeF :: Any -> nodeF (HashSet Node)
    anyToNodeF = unsafeCoerce

    anyToNodes :: Any -> HashSet Node
    anyToNodes = unsafeCoerce

    nodesToAny :: HashSet Node -> Any
    nodesToAny = unsafeCoerce

    tokToAny :: t -> Any
    tokToAny = unsafeCoerce

    completedMap :: HashMap Int (HashMap NT (HashMap Int (SemanticProd t)))
    completedMap = states
      <&> toList
      <&> filter (isCompleted rules)
      <&> fmap (\(DotProd ruleIdx _) -> (ruleIdx, Seq.index rules ruleIdx))
      <&> fmap (\(ruleIdx, Rule (EpsNT nt _) syms semantic) -> (nt, M.singleton ruleIdx $ gobbleNulling (syms, Seq.singleton semantic)))
      <&> M.fromListWith (<>)

    -- | See if the last non-nullable symbol is of the given kind, and if so produce a new
    -- 'SemanticProd' that removes the suffix (symbol <| nulling) and makes the corresponding
    -- updates to the possible semantic values
    endsWith :: Sym EpsNT t -> Seq Any -> SemanticProd t -> Maybe (SemanticProd t)
    endsWith sym alts (syms :|> sym', semantics)
      | sym == sym' = (syms, anyToFunc <$> semantics <*> alts) & gobbleNulling & Just
    endsWith _ _ _ = Nothing

    gobbleNulling :: SemanticProd t -> SemanticProd t
    gobbleNulling (syms, semantics) = (rest, foldr applyNulling semantics nullingSuffix)
      where
        (nullingSuffix, rest) = spanMaybeR nullNt syms
        nullNt (Nt (EpsNT nt Nulling)) = Just nt
        nullNt _ = Nothing
        applyNulling nt sems = anyToFunc <$> sems <*> M.lookupDefault mempty nt epsMap

    -- | Given a successful parse, construct the possible derivations as a DAG.
    doConstruct :: Vector t -> [Eim] -> Seq InactiveSet -> (HashMap Node (nodeF (HashSet Node)), HashSet Node)
    doConstruct input finalEims oldSets = runST $ do
      state@DerivationState{nodes} <- DerivationState <$> newSTRef 1 <*> newSTRef mempty <*> newSTRef mempty
      topNodes <- (`runReaderT` state) $
        finalEims <&> (\(Eim s _) -> s)
        & S.fromList
        & derivEims mempty startNt (0, finalIdx)
        <&> foldMap anyToNodes
      STRef.readSTRef nodes
        <&> M.union epsNodes
        <&> (\ns -> (ns, topNodes))
      where
        finalIdx = Seq.length oldSets - 1

        finishSemantics :: Int -> Int -> NT -> Seq Any -> DerivM s t nodeF (Seq Any)
        finishSemantics originIdx setIdx (NT _ ntkind) sems = case ntkind of
          NtNormal -> return sems
          NtMerge -> foldMap anyToNodes sems & nodesToAny & Seq.singleton & return
          NtRanged -> let range = (input Vector.! originIdx, input Vector.! (setIdx - 1))
                      in anyToRangeFunc <$> sems <*> pure (Just range) & return
          NtNode -> do
            nodes <- sems <&> anyToNodeF & mapM mkNode
            foldMap S.singleton nodes & nodesToAny & Seq.singleton & return

        mkNode :: nodeF (HashSet Node) -> DerivM s t nodeF Node
        mkNode n = do
          DerivationState{nextNode, nodes} <- ask
          nodeId <- readSTRef nextNode <* modifySTRef' nextNode (+ 1)
          modifySTRef' nodes $ M.insert (Node nodeId) n
          return $ Node nodeId

        -- | Produce the semantic result for the given 'NT', covering the given range, knowing
        -- that the 'Eim's from which it is to be derived have exactly the states given in the
        -- third argument
        derivEims :: HashMap Eim ReduceLinks -> NT -> (Int, Int) -> HashSet Int -> DerivM s t nodeF (Seq Any)
        derivEims extraLinks nt (originIdx, setIdx) ss = do
          DerivationState{alreadyDone} <- ask
          readSTRef alreadyDone <&> M.lookup (nt, originIdx, setIdx) >>= \case
            Just res -> return res
            Nothing -> do
              let eims = S.map (\s -> Eim s originIdx) ss
                  -- | The 'SemanticProd's that are completed in the active states
                  completed = foldMap ((`M.lookup` completedMap) >=> M.lookup nt) ss
                    & fold
                    & toList
                    & Seq.fromList
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
              scanRes <- if S.null scanPredSs then return mempty else do
                let tok = input Vector.! (setIdx-1)
                    sym = getKind @t tok & Sym
                toList completed
                  & mapMaybe (endsWith sym $ Seq.singleton $ tokToAny tok)
                  & Seq.fromList
                  & contDeriv (originIdx, setIdx-1) scanPredSs
              -- Compute the results produced by stepping over a non-terminal, and inserting the
              -- result of that non-terminal as a child
              reduceRes <- forM (M.toList reduceCauses) $ \((nt', predIdx), (predSs, childSs)) -> do
                children <- derivEims finalExtras nt' (predIdx, setIdx) childSs
                toList completed
                  & mapMaybe (endsWith (Nt $ EpsNT nt' NonNullable) children)
                  & Seq.fromList
                  & contDeriv (originIdx, predIdx) predSs
              -- Wrap everything up by performing the final action dictated by the non-terminal
              res <- scanRes : reduceRes
                & fold
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
                  eimNt = M.lookup nextS completedMap & fold & void & S.fromMap & mkSingle
                  addNewLink = M.insertWith (M.unionWith (<>)) eim $
                    M.singleton (nt, origin) (S.singleton parentS, S.singleton s)
              in go (addNewLink links) (eimNt, eim)
            fromJust (Just a) = a
            fromJust _ = error "fromJust failed in limChain"
            mkSingle (S.toList -> [a]) = a
            mkSingle _ = error "Got more than one result when trying to construct a Lim chain"

        contDeriv :: (Int, Int) -> HashSet Int -> Seq (SemanticProd t) -> DerivM s t nodeF (Seq Any)
        contDeriv (originIdx, setIdx) _ semProds
          | setIdx == originIdx = foldMap snd semProds & return
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
          scanRes <- if S.null scanPredSs then return mempty else do
            let tok = input Vector.! (setIdx-1)
                sym = getKind @t tok & Sym
            toList semProds
              & mapMaybe (endsWith sym $ Seq.singleton $ tokToAny tok)
              & Seq.fromList
              & contDeriv (originIdx, setIdx-1) scanPredSs
          -- Compute the results produced by stepping over a non-terminal, and inserting the
          -- result of that non-terminal as a child
          reduceRes <- forM (M.toList reduceCauses) $ \((nt', predIdx), (predSs, childSs)) -> do
            children <- derivEims finalExtras nt' (predIdx, setIdx) childSs
            toList semProds
              & mapMaybe (endsWith (Nt $ EpsNT nt' NonNullable) children)
              & Seq.fromList
              & contDeriv (originIdx, predIdx) predSs
          scanRes : reduceRes
            & fold
            & return

spanMaybeR :: (a -> Maybe b) -> Seq a -> (Seq b, Seq a)
spanMaybeR f = Seq.spanr (f >>> isJust) >>> first (fmap $ f >>> fromJust)
  where
    fromJust (Just a) = a
    fromJust Nothing = error "spanMaybeR somehow got a thing that passed isJust, yet wasn't a Just"

newtype MonoidHashMap k v = MonoidHashMap (HashMap k v)

instance (Eq k, Hashable k, Semigroup v) => Semigroup (MonoidHashMap k v) where
  MonoidHashMap a <> MonoidHashMap b = MonoidHashMap $ M.unionWith (<>) a b

instance (Eq k, Hashable k, Semigroup v) => Monoid (MonoidHashMap k v) where
  mempty = MonoidHashMap mempty
  mappend = (<>)
