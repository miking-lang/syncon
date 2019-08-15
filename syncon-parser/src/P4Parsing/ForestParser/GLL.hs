{-# OPTIONS_GHC -fno-warn-orphans #-}

module P4Parsing.ForestParser.GLL (parse, Node) where

import Pre hiding (Symbol, Any, State)

import qualified Prelude as Prelude
import Unsafe.Coerce (unsafeCoerce)
import GHC.Exts (Any)

import Data.Array (Array, listArray, bounds)
import Data.STRef (STRef, newSTRef, readSTRef, modifySTRef')
import qualified Data.Text as Text
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as OM

import GLL.Parser (Parseable(..), Symbol(..), ParseResult(ParseResult, res_success, error_message, sppf_result))
import qualified GLL.Parser as GLL

import P4Parsing.ForestParser.Grammar

-- interface

newtype Node = Node Int deriving (Eq, Hashable)

-- | Parse a sequence given a grammar. The underlying parser doesn't give an easily inspectable error,
-- hence we just pass that on, for the moment. In the success case you get a tuple containing the nodes
-- in the parse forest and a sequence of root nodes.
parse :: forall t nodeF. (Show t, Ord t, Eq t, Hashable t)
      => (forall r. Grammar r t nodeF r)
      -> (forall f. Foldable f => f t -> Either Text (HashMap Node (nodeF (HashSet Node)), HashSet Node))  -- TODO: might want to use a 'ghosts of departed proofs' approach to the map and the nodes
parse unfixedGrammar = go
  where
    go :: Foldable f => f t -> Either Text (HashMap Node (nodeF (HashSet Node)), HashSet Node)
    go foldable = input <&> Concrete & gll <&> repack input <&> buildDAG
      where
        input = listArray (0, length foldable - 1) (toList foldable)

    gll :: Array Int (Token t) -> Either Text (GLL.PackMap (Token t))
    gll = GLL.parseWithOptionsArray [GLL.packedNodesOnly] grammar >>> \case
      ParseResult{res_success = False, error_message} -> Left $ toS error_message
      ParseResult{res_success = True, sppf_result = (_, _, pm, _)} -> Right pm

    (semantics, grammar) = runST $ do
      initialState@State{prods} <- State <$> newSTRef 0 <*> newSTRef M.empty <*> newSTRef S.empty
      WrappedNt startNt <- runReaderT (runGrammar mkRule unfixedGrammar >>= buildWrappedProd) initialState
      prodMap <- readSTRef prods
      return (prodMap, (startNt, M.keys prodMap <&> toGLLProd))

    repack :: Array Int t -> GLL.PackMap (Token t) -> PackedForest t
    repack input pm = PackedForest{input, semantics, uncompleted, completed}
      where
        uncompleted = [IM.singleton l $ IM.singleton r $ IM.singleton d $ M.singleton arrProd v
                      | (l, rdpv) <- IM.toList pm
                      , (r, dpv) <- IM.toList rdpv
                      , (d, pv) <- IM.toList dpv
                      , (p, v) <- OM.toList pv
                      , let arrProd = fromGLLProd p
                      , isComplete d arrProd]
          & IM.unionsWith (IM.unionWith $ IM.unionWith $ M.unionWith IS.union)
        completed = [IM.singleton l $ IM.singleton r $ M.singleton arrProd v
                      | (l, rdpv) <- IM.toList pm
                      , (r, dpv) <- IM.toList rdpv
                      , (d, pv) <- IM.toList dpv
                      , (p, v) <- OM.toList pv
                      , let arrProd = fromGLLProd p
                      , not $ isComplete d arrProd]
          & IM.unionsWith (IM.unionWith $ M.unionWith IS.union)
        isComplete idx (ArrProd _ arr) = (bounds arr & (\(l, r) -> r - l + 1)) == idx

    buildDAG :: PackedForest t -> (HashMap Node (nodeF (HashSet Node)), HashSet Node)
    buildDAG = undefined  -- TODO

-- internal stuff

data PackedForest t = PackedForest
  { input :: !(Array Int t)
  , completed :: !(IntMap (IntMap (HashMap (ArrProd (Token t)) IntSet))) -- ^ left extent -> right extent -> prod -> pivots
  , uncompleted :: !(IntMap (IntMap (IntMap (HashMap (ArrProd (Token t)) IntSet)))) -- ^ left extent -> right extent -> dot_idx -> prod -> pivots
  , semantics :: !(HashMap (ArrProd (Token t)) Any)
  }

buildWrappedProd :: (Eq t, Hashable t) => WrappedProd t nodeF -> GrammarM s t Nt
buildWrappedProd (WrappedProd nt prod) = do
  State{alreadyStarted} <- ask
  started <- lift $ readSTRef alreadyStarted <&> S.member nt
  if started then return nt else do
    lift $ modifySTRef' alreadyStarted $ S.insert nt
    (syms, semantic) <- buildProd prod
    addProd nt syms semantic
    return nt

buildProd :: (Eq t, Hashable t) => Prod (WrappedProd t nodeF) t nodeF a -> GrammarM s t ([Symbol (Token t)], Any)
buildProd (Pure a) = return ([], toAny a)
buildProd (Terminal f cont) = buildProd cont <&> first (mkTok f :)
buildProd (NonTerminal wp cont) = do
  wpnt <- buildWrappedProd wp
  buildProd cont <&> first (mkNtTok wpnt :)
buildProd (Ambig ps cont) = do
  nt <- freshMerge
  forM_ ps $ \p -> do
    (syms, semantic) <- buildProd p
    addProd nt syms semantic
  buildProd cont <&> first (mkNtTok nt :)
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

data WrappedProd t nodeF = WrappedProd Nt (Prod (WrappedProd t nodeF) t nodeF (nodeF (WrappedProd t nodeF)))

mkRule :: Prod (WrappedProd t nodeF) t nodeF (nodeF (WrappedProd t nodeF)) -> GrammarM s t (WrappedProd t nodeF)
mkRule p = do
  nt <- freshNonMerge
  return $ WrappedProd nt p

type GrammarM s t a = ReaderT (State s t) (ST s) a
data State s t = State
  { nextNt :: !(STRef s Int)
  , prods :: !(STRef s (HashMap (ArrProd (Token t)) Any))
  , alreadyStarted :: !(STRef s (HashSet Nt)) }

mergeChar, nonMergeChar :: Char
(mergeChar, nonMergeChar) = ('M', 'N')

-- | The type of non-terminals. Note that there are two kinds of nts: those that assume their
-- alternative productions produce 'HashSet Node' and union them, and those that make no assumption
-- about the produced type and combine results as in the list monad. (The latter multiplies the number
-- of things produced, the former leaves it unchanged)
newtype Nt = WrappedNt Text deriving (Eq, Hashable)  -- Following non-terminals in gll

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

anyToNode :: Any -> HashSet Node
anyToNode = unsafeCoerce

mkTok :: (t -> Bool) -> Symbol (Token t)
mkTok = Conditional >>> Term

mkNtTok :: Nt -> Symbol t
mkNtTok (WrappedNt nt) = Nt nt

data ArrProd t = ArrProd Nt (Array Int (Symbol t)) deriving (Generic, Eq)
instance Hashable t => Hashable (ArrProd t) where
  hashWithSalt = hashUsing $ \(ArrProd nt arr) -> (nt, toList arr)

mkArrProd :: Nt -> [Symbol t] -> ArrProd t
mkArrProd nt syms = ArrProd nt $ listArray (0, length syms - 1) syms

toGLLProd :: ArrProd t -> GLL.Prod t
toGLLProd (ArrProd (WrappedNt nt) syms) = GLL.Prod nt $ toList syms

fromGLLProd :: GLL.Prod t -> ArrProd t
fromGLLProd (GLL.Prod nt syms) = mkArrProd (WrappedNt nt) syms

-- NOTE: these are orphan instances, since writing a wrapper and manually writing hashable instances for
-- those are very annoying.
deriving instance Generic (Symbol t)
instance Hashable t => Hashable (Symbol t)

data Token t = Concrete t
             | Eof
             | Eps
             | Conditional (t -> Bool)
data TokenBland t = ConcreteBland t | EofBland | EpsBland | ConditionalBland deriving Generic
instance Hashable t => Hashable (TokenBland t)

instance Hashable t => Hashable (Token t) where
  hashWithSalt = hashUsing $ \case
    (Concrete t) -> ConcreteBland t
    Eof -> EofBland
    Eps -> EpsBland
    Conditional{} -> ConditionalBland

instance Show t => Show (Token t) where
  show (Concrete t) = "(Concrete " <> show t <> ")"
  show Eof = "Eof"
  show Eps = "Eps"
  show Conditional{} = "Conditional"

instance Eq t => Eq (Token t) where
  Concrete a == Concrete b = a == b
  Eof == Eof = True
  Eps == Eps = True
  Conditional _ == Conditional _ = True
  _ == _ = False

-- TODO: confirm that this is actually consistent, I wrote it kinda quickly
instance Ord t => Ord (Token t) where
  compare (Concrete a) (Concrete b) = compare a b
  compare a b | a == b = EQ
  compare Concrete{} _ = LT
  compare Eof Concrete{} = GT
  compare Eof _ = LT
  compare Eps Concrete{} = GT
  compare Eps Eof = GT
  compare Eps _ = LT
  compare Conditional{} _ = GT

-- NOTE: I really don't like this library, it requires too many instances that do not feel like they make sense here
instance (Show t, Ord t, Eq t) => Parseable (Token t) where
  eos = Eof
  eps = Eps  -- NOTE: as far as I can tell this isn't actually used by the library, but it requires its existence...
  matches (Concrete t) (Conditional f) = f t
  matches (Conditional f) (Concrete t) = f t
  matches a b = a == b
  unlex (Concrete t) = show t  -- TODO: something better than show
  unlex Eof = "<end-of-file>"
  unlex Eps = "<empty-string>"
  unlex Conditional{} = "<func>"
