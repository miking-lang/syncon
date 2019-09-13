{-# OPTIONS_GHC -fno-warn-orphans #-}

module P4Parsing.ForestParser.GLL (parse, Node) where

import Pre hiding (Symbol, Any, State)

import qualified Prelude as Prelude
import Unsafe.Coerce (unsafeCoerce)
import GHC.Exts (Any)

import Data.Array (Array, listArray, bounds)
import qualified Data.Array as Array
import Data.STRef (STRef, newSTRef, readSTRef, modifySTRef')
import qualified Data.Text as Text
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as OM
import qualified Data.Sequence as Seq

import GLL.Parser (Parseable(..), Symbol(..), ParseResult(ParseResult, res_success, error_message, sppf_result))
import qualified GLL.Parser as GLL

import P4Parsing.ForestParser.Grammar
import P4Parsing.ForestParser.Grammar as Grammar

-- interface

newtype Node = Node Int deriving (Eq, Hashable, Show, NFData)

-- | Parse a sequence given a grammar. The underlying parser doesn't give an easily inspectable error,
-- hence we just pass that on, for the moment. In the success case you get a tuple containing the nodes
-- in the parse forest and a sequence of root nodes.
parse :: forall t nodeF. (Show t, Ord t, Hashable t, Unlexable t)
      => (forall r. Grammar r t nodeF (Prod r t nodeF r))
      -> (forall f. Foldable f => f t -> Either Text (HashMap Node (nodeF (HashSet Node)), HashSet Node))  -- TODO: might want to use a 'ghosts of departed proofs' approach to the map and the nodes
parse unfixedGrammar = go
  where
    go :: Foldable f => f t -> Either Text (HashMap Node (nodeF (HashSet Node)), HashSet Node)
    go foldable = toList input <&> Concrete & gll <&> repack input <&> buildDAG
      where
        input = listArray (0, length foldable - 1) (toList foldable)

    gllParseFunction = GLL.parseWithOptions [GLL.packedNodesOnly] grammar

    gll :: [Token t] -> Either Text (GLL.PackMap (Token t))
    gll = toList >>> gllParseFunction >>> \case  -- NOTE: gll has a parseArray variant, but it requires you to have already added an Eof element, so it's easier to make it into a list temporarily here
      ParseResult{res_success = False, error_message} -> Left $ toS error_message
      ParseResult{res_success = True, sppf_result = (_, _, pm, _)} -> Right pm

    (semantics, grammar) = runST $ do
      initialState@State{prods} <- State <$> newSTRef 0 <*> newSTRef M.empty <*> newSTRef S.empty
      WrappedNt startNt <- runReaderT (runGrammar mkRule mkAmbig unfixedGrammar >>= buildProd >>= mkStart) initialState
      prodMap <- readSTRef prods
      return (prodMap, (startNt, M.keys prodMap <&> toGLLProd))
    mkStart (syms, sem) = do
      startNt <- freshNonMerge
      addProd startNt syms sem
      return startNt

    repack :: Array Int t -> GLL.PackMap (Token t) -> PackedForest t
    repack input pm = PackedForest{input, semantics, uncompleted, completed}
      where
        uncompleted = [IM.singleton l $ IM.singleton r $ IM.singleton d $ M.singleton arrProd v
                      | (l, rdpv) <- IM.toList pm
                      , (r, dpv) <- IM.toList rdpv
                      , (d, pv) <- IM.toList dpv
                      , (p, v) <- OM.toList pv
                      , let arrProd = fromGLLProd p
                      , not $ isComplete d arrProd]
          & IM.unionsWith (IM.unionWith $ IM.unionWith $ M.unionWith IS.union)
        completed = [IM.singleton l $ IM.singleton r $ M.singleton nt $ Seq.singleton (arrProd, v)
                      | (l, rdpv) <- IM.toList pm
                      , (r, dpv) <- IM.toList rdpv
                      , (d, pv) <- IM.toList dpv
                      , (p, v) <- OM.toList pv
                      , let arrProd@(ArrProd nt _) = fromGLLProd p
                      , isComplete d arrProd]
          & IM.unionsWith (IM.unionWith $ M.unionWith mappend)
        isComplete idx (ArrProd _ arr) = arrLen arr == idx

    buildDAG :: PackedForest t -> (HashMap Node (nodeF (HashSet Node)), HashSet Node)
    buildDAG pf@PackedForest{input} = runST $ do
      initialState@DagState{nodeMap} <- DagState pf <$> newSTRef M.empty <*> newSTRef 0 <*> newSTRef M.empty
      topNodes <- runReaderT (dagNt (fst grammar & WrappedNt, 0, arrLen input)) initialState <&> foldMap anyToNodeSet
      finalNodeMap <- readSTRef nodeMap
      return (finalNodeMap, topNodes)

-- internal stuff

data PackedForest t = PackedForest
  { input :: !(Array Int t)
  , completed :: !(IntMap (IntMap (HashMap Nt (Seq ((ArrProd (Token t)), IntSet))))) -- ^ left extent -> right extent -> prod -> pivots
  , uncompleted :: !(IntMap (IntMap (IntMap (HashMap (ArrProd (Token t)) IntSet)))) -- ^ left extent -> right extent -> dot_idx -> prod -> pivots
  , semantics :: !(HashMap (ArrProd (Token t)) Any)
  }

data DagState s t nodeF = DagState
  { packedForest :: !(PackedForest t)
  , nodeMap :: !(STRef s (HashMap Node (nodeF (HashSet Node))))
  , nextNodeId :: !(STRef s Int)
  , translation :: !(STRef s (HashMap (Nt, Int, Int) (Seq Any)))
  }

type DagM s t nodeF a = ReaderT (DagState s t nodeF) (ST s) a

dagNt :: (Eq t, Hashable t, Show t) => (Nt, Int, Int) -> DagM s t nodeF (Seq Any)
dagNt pos@(nt, l, r) = do
  DagState{translation, packedForest = PackedForest{completed}} <- ask
  lift (readSTRef translation <&> M.lookup pos) >>= \case
    Just res -> return res
    Nothing -> do
      prods <- IM.lookup l completed >>= IM.lookup r >>= M.lookup nt & fold & mapM (dagProd l r)
      result <- maybeRanged $ maybeMerge $ join prods
      lift $ modifySTRef' translation $ M.insert pos result
      return result
  where
    maybeMerge
      | Text.head (coerce nt) == mergeChar = toList >>> fmap anyToNodeSet >>> S.unions >>> Seq.singleton >>> fmap toAny
      | otherwise = identity
    maybeRanged res
      | Text.head (coerce nt) == rangeChar = do
          range <- if l == r then return Nothing
            else asks (packedForest >>> input)
                 <&> ((Array.! l) &&& (Array.! (r-1)))
                 <&> Just
          res <&> anyToRangeFunc <&> ($ range) & return
      | otherwise = return res
    anyToRangeFunc :: Any -> (Maybe (t, t) -> Any)
    anyToRangeFunc = unsafeCoerce

dagProd :: forall s t nodeF. (Eq t, Hashable t, Show t)
        => Int -> Int -> (ArrProd (Token t), IntSet)
        -> DagM s t nodeF (Seq Any)
dagProd l r (prod@(ArrProd nt syms), pivots)
  | arrLen syms == 0 = getSem <&> Seq.singleton
  | otherwise = do
      sem <- getSem <&> anyToFunc
      results <- fmap join $ forM (IS.toList pivots & Seq.fromList) $ \pivot -> do
        rightMosts <- dagSym pivot r $ syms Array.! (dotIdx - 1)
        dagIProd l pivot (prod, dotIdx - 1) $ sem <$> rightMosts
      if not isNode then return results else do
        nodes <- forM results $ anyToNode >>> mkNode
        return $ Seq.singleton $ toAny $ foldMap S.singleton nodes
  where
    dotIdx = arrLen syms
    getSem = asks $ packedForest >>> semantics >>> M.lookup prod >>> fromJust
    fromJust = compFromJust "P4Parsing.ForestParser.GLL.dagProd" $ "Somehow did not enter semantics for " <> show prod
    isNode = coerce nt & Text.head & (== nodeChar)
    anyToNode :: Any -> nodeF (HashSet Node)
    anyToNode = unsafeCoerce

dagSym :: (Eq t, Hashable t, Show t) => Int -> Int -> Symbol (Token t) -> DagM s t nodeF (Seq Any)
dagSym l _r (Term _) = do
  PackedForest{input} <- asks packedForest
  return $ Seq.singleton $ toAny $ input Array.! l
dagSym l r (Nt nt) = dagNt (WrappedNt nt, l, r)

dagIProd :: (Eq t, Hashable t, Show t)
         => Int -> Int -> (ArrProd (Token t), Int) -> Seq Any
         -> DagM s t nodeF (Seq Any)
dagIProd _ _ (_, 0) sems = return sems
dagIProd l r (prod@(ArrProd _ syms), dotIdx) sems = do
  PackedForest{uncompleted} <- asks packedForest
  let pivots = if dotIdx /= 1
        then IM.lookup l uncompleted >>= IM.lookup r >>= IM.lookup dotIdx >>= M.lookup prod & fold
        else IS.singleton l
  fmap join $ forM (IS.toList pivots & Seq.fromList) $ \pivot -> do
    rightMosts <- dagSym pivot r $ syms Array.! (dotIdx - 1)
    dagIProd l pivot (prod, dotIdx - 1) $ anyToFunc <$> sems <*> rightMosts

mkNode :: nodeF (HashSet Node) -> DagM s t nodeF Node
mkNode node = do
  DagState{nextNodeId, nodeMap} <- ask
  id <- lift $ readSTRef nextNodeId <* modifySTRef' nextNodeId (+1)
  lift $ modifySTRef' nodeMap $ M.insert (Node id) node
  return $ Node id

buildWrappedProd :: (Eq t, Hashable t) => WrappedProd t nodeF -> GrammarM s t Nt
buildWrappedProd (WrappedProd nt prod) = do
  State{alreadyStarted} <- ask
  started <- lift $ readSTRef alreadyStarted <&> S.member nt
  if started then return nt else do
    lift $ modifySTRef' alreadyStarted $ S.insert nt
    (syms, semantic) <- buildProd prod
    addProd nt syms semantic
    return nt
buildWrappedProd (WrappedProds nt prods) = do
  State{alreadyStarted} <- ask
  started <- lift $ readSTRef alreadyStarted <&> S.member nt
  if started then return nt else do
    lift $ modifySTRef' alreadyStarted $ S.insert nt
    forM_ prods $ \prod -> do
      (syms, semantic) <- buildProd prod
      addProd nt syms semantic
    return nt

buildProd :: (Eq t, Hashable t) => Prod (WrappedProd t nodeF) t nodeF a -> GrammarM s t ([Symbol (Token t)], Any)
buildProd (Pure a) = return ([], toAny a)
buildProd (Terminal label f cont) = buildProd cont <&> first (mkTok label f :)
buildProd (NonTerminal wp cont) = do
  wpnt <- buildWrappedProd wp
  buildProd cont <&> first (mkNtTok wpnt :)
buildProd (Ranged r cont) = do
  nt <- freshRange
  (syms, semantic) <- buildProd r
  addProd nt syms semantic
  buildProd cont <&> first (mkNtTok nt :)
buildProd (Alts [] _) = return $ ([mkTok "<never>" (const False)], toAny identity)
buildProd (Alts ps cont) = do
  nt <- freshNonMerge
  forM_ ps $ \p -> do
    (syms, semantic) <- buildProd p
    addProd nt syms semantic
  buildProd cont <&> first (mkNtTok nt :)
buildProd (Many p cont) = do
  nt1 <- freshNonMerge
  nt2 <- freshNonMerge
  addProd nt1 [] (toAny [])
  addProd nt1 [mkNtTok nt2, mkNtTok nt1] (toAny $ flip (:))
  (syms, semantic) <- buildProd p
  addProd nt2 syms semantic
  buildProd cont <&> first (mkNtTok nt1 :)

data WrappedProd t nodeF
  = WrappedProd Nt (Prod (WrappedProd t nodeF) t nodeF (nodeF (WrappedProd t nodeF)))
  | WrappedProds Nt [Prod (WrappedProd t nodeF) t nodeF (WrappedProd t nodeF)]

mkRule :: Prod (WrappedProd t nodeF) t nodeF (nodeF (WrappedProd t nodeF)) -> GrammarM s t (Prod (WrappedProd t nodeF) t nodeF (WrappedProd t nodeF))
mkRule p = do
  nt <- freshNode
  return $ NonTerminal (WrappedProd nt p) $ pure identity

mkAmbig :: [Prod (WrappedProd t nodeF) t nodeF (WrappedProd t nodeF)] -> GrammarM s t (Prod (WrappedProd t nodeF) t nodeF (WrappedProd t nodeF))
mkAmbig ps = do
  nt <- freshMerge
  return $ NonTerminal (WrappedProds nt ps) $ pure identity

type GrammarM s t a = ReaderT (State s t) (ST s) a
data State s t = State
  { nextNt :: !(STRef s Int)
  , prods :: !(STRef s (HashMap (ArrProd (Token t)) Any))
  , alreadyStarted :: !(STRef s (HashSet Nt)) }

mergeChar, nonMergeChar, nodeChar, rangeChar :: Char
(mergeChar, nonMergeChar, nodeChar, rangeChar) = ('M', 'n', 'N', 'r')

-- | The type of non-terminals. Note that there are two kinds of nts: those that assume their
-- alternative productions produce 'HashSet Node' and union them, and those that make no assumption
-- about the produced type and combine results as in the list monad. (The latter multiplies the number
-- of things produced, the former leaves it unchanged)
newtype Nt = WrappedNt Text deriving (Eq, Show, Hashable)  -- Following non-terminals in gll

freshNonMerge :: GrammarM s t Nt
freshNonMerge = do
  State{nextNt} <- ask
  id <- lift $ readSTRef nextNt <* modifySTRef' nextNt (+1)
  return $ WrappedNt $ Text.singleton nonMergeChar <> show id

freshMerge :: GrammarM s t Nt
freshMerge = do
  State{nextNt} <- ask
  id <- lift $ readSTRef nextNt <* modifySTRef' nextNt (+1)
  return $ WrappedNt $ Text.singleton mergeChar <> show id

freshNode :: GrammarM s t Nt
freshNode = do
  State{nextNt} <- ask
  id <- lift $ readSTRef nextNt <* modifySTRef' nextNt (+1)
  return $ WrappedNt $ Text.singleton nodeChar <> show id

freshRange :: GrammarM s t Nt
freshRange = do
  State{nextNt} <- ask
  id <- lift $ readSTRef nextNt <* modifySTRef' nextNt (+1)
  return $ WrappedNt $ Text.singleton rangeChar <> show id

-- | Add a production. The 'Any' argument should be a function that takes the values produced by
-- each 'Symbol', one at a time, in reverse order. Note that this interface is massively unsafe,
-- you really have to make sure that the function is correct. Note that if the list of symbols is
-- empty then the production matches the empty string and the 'Any' argument will just be returned
-- as is.
--
-- Examples:
-- addProd [a, b, c] (toAny (\c b a -> ret))
-- addProd [] (toAny ret)
addProd :: (Eq t, Hashable t) => Nt -> [Symbol (Token t)] -> Any -> GrammarM s t ()
addProd nt syms semantics = do
  State{prods} <- ask
  lift $ modifySTRef' prods $ M.insert (mkArrProd nt syms) semantics

toAny :: a -> Any
toAny = unsafeCoerce

anyToFunc :: Any -> (Any -> Any)
anyToFunc = unsafeCoerce

anyToNodeSet :: Any -> HashSet Node
anyToNodeSet = unsafeCoerce

mkTok :: Text -> (t -> Bool) -> Symbol (Token t)
mkTok label = Conditional label >>> Term

mkNtTok :: Nt -> Symbol t
mkNtTok (WrappedNt nt) = Nt nt

data ArrProd t = ArrProd Nt (Array Int (Symbol t)) deriving (Generic, Eq, Show)
instance Hashable t => Hashable (ArrProd t) where
  hashWithSalt = hashUsing $ \(ArrProd nt arr) -> (nt, toList arr)

mkArrProd :: Nt -> [Symbol t] -> ArrProd t
mkArrProd nt syms = ArrProd nt $ listArray (0, length syms - 1) syms

toGLLProd :: ArrProd t -> GLL.Prod t
toGLLProd (ArrProd (WrappedNt nt) syms) = GLL.Prod nt $ toList syms

fromGLLProd :: GLL.Prod t -> ArrProd t
fromGLLProd (GLL.Prod nt syms) = mkArrProd (WrappedNt nt) syms

arrLen :: Array Int a -> Int
arrLen = bounds >>> \(l, r) -> r - l + 1

-- NOTE: these are orphan instances, since writing a wrapper and manually writing hashable instances for
-- those are very annoying.
deriving instance Generic (Symbol t)
instance Hashable t => Hashable (Symbol t)

data Token t = Concrete t
             | Eof
             | Eps
             | Conditional Text (t -> Bool)
data TokenBland t = ConcreteBland t | EofBland | EpsBland | ConditionalBland Text deriving (Eq, Ord, Generic)
instance Hashable t => Hashable (TokenBland t)

toBland :: Token t -> TokenBland t
toBland (Concrete t) = ConcreteBland t
toBland Eof = EofBland
toBland Eps = EpsBland
toBland (Conditional label _) = ConditionalBland label

instance Hashable t => Hashable (Token t) where
  hashWithSalt = hashUsing toBland

instance Show t => Show (Token t) where
  show (Concrete t) = "(Concrete " <> show t <> ")"
  show Eof = "Eof"
  show Eps = "Eps"
  show (Conditional label _) = "(Conditional " <> show label <> " <func>)"

instance Eq t => Eq (Token t) where
  a == b = toBland a == toBland b

instance Ord t => Ord (Token t) where
  compare a b = compare (toBland a) (toBland b)

-- NOTE: I really don't like this library, it requires too many instances that do not feel like they make sense here
instance (Unlexable t, Show t, Ord t, Eq t) => Parseable (Token t) where
  eos = Eof
  eps = Eps  -- NOTE: as far as I can tell this isn't actually used by the library, but it requires its existence...
  matches (Concrete t) (Conditional _ f) = f t
  matches (Conditional _ f) (Concrete t) = f t
  matches a b = a == b
  unlex (Concrete t) = toS $ Grammar.unlex t
  unlex Eof = "<end-of-file>"
  unlex Eps = "<empty-string>"
  unlex (Conditional label _) = toS label
