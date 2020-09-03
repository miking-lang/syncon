module P5DynamicAmbiguity.Isolation
( isolate
, dummyIsolate
, getElidable
, showElidable
, getElidableBoundsEx
, getNodeOrElidableBoundsEx
, Elidable
, isAccepted
) where

import Pre hiding (reduce, State, state, orElse)
import Result (Result(..))

import qualified Data.HashSet as S
import qualified Data.HashMap.Lazy as M
import qualified Data.Sequence as Seq
import Data.STRef (STRef, newSTRef, readSTRef, modifySTRef')

import Data.Functor.Foldable (embed, project)
import Text.Earley.Forest.Grammar (unlex, Parseable)
import Text.Earley.Forest.Parser (Node)

import P1Lexing.Types (Range)
import P2LanguageDefinition.Types (TypeName(..), Name)
import P4Parsing.Types (NodeF(NodeF), n_nameF, n_rangeF, n_beginEndF)
import qualified P4Parsing.Types as P4
import P5DynamicAmbiguity.Types hiding (NodeOrElide)
import P5DynamicAmbiguity.TreeLanguage (PreLanguage(..), getSyTy)
import qualified P5DynamicAmbiguity.Types as P5

-- TODO: I suspect that only (HashSet Node) will be chosen for elison, check this
type Elidable = Either Node (HashSet Node)
type NodeOrElide = P5.NodeOrElide Elidable

type Dag tok = HashMap Node (NodeF tok (HashSet Node))

data State s tok = State
  { dag :: !(Dag tok)
  , validElisonsMemo :: !(STRef s (HashMap Elidable (HashSet Elidable)))  -- ^ Memoized results of the elidable analysis
  }
type IsolationM s tok a = ReaderT (State s tok) (ST s) a

type FullNode = P4.Node
type Res tok = Result (HashMap (HashSet Node) (Seq (NodeOrElide tok)))

isAccepted :: HashSet (HashSet Name) -> Seq (NodeOrElide tok) -> Bool
isAccepted accepted = foldMap getNames >>> (`S.member` accepted)
  where
    getNames :: NodeOrElide tok -> HashSet Name
    getNames (Node n) = S.insert (n_nameF n) $ foldMap getNames n
    getNames Elide{} = S.empty

-- TODO: Bail out if a single ambiguity gets too large
-- | Produce a single parse tree, or a disjoint set of minimal ambiguities.
isolate :: (Dag tok, HashSet Node)
        -> Result (Seq (Seq (NodeOrElide tok))) (FullNode tok)
isolate (dag, roots) = runST $ do
  state <- State dag <$> newSTRef M.empty
  runReaderT (amb roots) state <&> first (toList >>> Seq.fromList)

getElidable :: PreLanguage Elidable -> Dag tok -> Elidable -> (Range, TypeName)
getElidable pl dag = fmap toList >>> \case
  Left n -> getNode n & \NodeF{n_nameF, n_rangeF} -> (n_rangeF, getSyTy pl n_nameF)
  Right (n : _) -> getNode n & \NodeF{n_nameF, n_rangeF} -> (n_rangeF, getSyTy pl n_nameF)
  Right [] -> compErr "P5DynamicAmbiguity.Analysis.getElidable" "Elided an empty ambiguity node"
  where
    getNode n = M.lookup n dag
      & compFromJust "P5DynamicAmbiguity.Analysis.getElidable.getNode" ("Missing node " <> show n)

showElidable :: (Eq tok, Parseable tok) => Dag tok -> Elidable -> Text
showElidable dag = fmap toList >>> \case
  Left n -> formatNode n
  Right (n : _) -> formatNode n
  Right [] -> compErr "P5DynamicAmbiguity.Analysis.showElidable" "Elided an empty ambiguity node"
  where
    formatNode :: Node -> Text
    formatNode = getNode >>> n_beginEndF >>> \case
      Nothing -> ""
      Just (b, e) | b == e -> unlex b
      Just _ -> "..."
    getNode n = M.lookup n dag
      & compFromJust "P5DynamicAmbiguity.Analysis.showElidable.getNode" ("Missing node " <> show n)

getElidableBoundsEx :: Dag tok -> Elidable -> (tok, tok)
getElidableBoundsEx dag = fmap toList >>> \case
  Left n -> getNodeBounds n
  Right (n : _) -> getNodeBounds n
  Right [] -> compErr "P5DynamicAmbiguity.Analysis.getBoundsEx" "Empty elidable"
  where
    getNodeBounds n = M.lookup n dag
      & compFromJust "P5DynamicAmbiguity.Analysis.getBoundsEx.getElidableBounds" ("Missing node " <> show n)
      & n_beginEndF
      & compFromJust "P5DynamicAmbiguity.Analysis.getBoundsEx.getElidableBounds" ("Missing bounds on node " <> show n)

getNodeOrElidableBoundsEx :: Show tok => Dag tok -> NodeOrElide tok -> (tok, tok)
getNodeOrElidableBoundsEx dag = \case
  Node n -> n_beginEndF n & compFromJust "P5DynamicAmbiguity.Analysis.getNodeOrElidableBoundsEx.getNodeBounds" ("Missing bounds on node " <> show n)
  Elide e -> getElidableBoundsEx dag e

-- | Given a potential ambiguity, construct either a complete subtree, or a disjoint set of
-- minimal ambiguities
amb :: HashSet Node -> IsolationM s tok (Res tok (FullNode tok))
amb nodes
  | [node] <- toList nodes = do
      fetchNode node >>= traverse amb <&> sequence <&> fmap embed
  | otherwise = do
      elidables <- validElidables (Right nodes) <&> S.delete (Right nodes)
      mkAmb elidables nodes <&> Error

-- | Construct the ambiguity rooted at the second argument, using the first argument as the
-- nodes that are valid elisons.
mkAmb :: forall s tok. HashSet Elidable -> HashSet Node -> IsolationM s tok (HashMap (HashSet Node) (Seq (NodeOrElide tok)))
mkAmb elidable roots = recurAmb roots
  <&> second (Seq.fromList >>> M.singleton roots >>> Error)
  <&> uncurry (<*)
  <&> fromError
  where
    -- recur and recurAmb are mutually recursive and perform the traversal that constructs the
    -- delimited parse trees that are a part of the current ambiguity. That's the second value
    -- in the returned tuple. The first value is a computation that has looked for ambiguities
    -- further down the AST.
    recurAmb :: HashSet Node -> IsolationM s tok (Res tok (), [NodeOrElide tok])
    recurAmb nodes
      | Right nodes `S.member` elidable = amb nodes <&> void <&> (,[Elide $ Right nodes])
      | otherwise = toList nodes & traverse recur <&> mconcat
    recur :: Node -> IsolationM s tok (Res tok (), [NodeOrElide tok])
    recur node
      | Left node `S.member` elidable = amb (S.singleton node) <&> void <&> (,[Elide $ Left node])
      | otherwise = fetchNode node >>= traverse recurAmb <&> sequence <&> second (sequence >>> fmap Node)

    fromError (Error e) = e
    fromError Data{} = compErr "P5DynamicAmbiguity.AmbiguityReporter2.mkAmb" "impossible"

-- | Given a particular root (most commonly an ambiguity node HashSet Node), find the set of nodes
-- that can be elided, i.e., the nodes that are present in all alternatives.
validElidables :: Elidable -> IsolationM s tok (HashSet Elidable)
validElidables top = do
  ref <- asks validElisonsMemo
  mTop <- lift $ readSTRef ref <&> M.lookup top
  orElse mTop $ save ref $ case top of
    Left n -> fetchNode n <&> toList
      >>= traverse (Right >>> validElidables)
      <&> S.unions <&> S.insert top
    Right ns -> toList ns
      & traverse (Left >>> validElidables)
      <&> intersections <&> S.insert top
  where
    orElse (Just a) _ = return a
    orElse Nothing m = m
    save ref res = res >>= \res' -> lift $ modifySTRef' ref (M.insert top res') *> return res'
    intersections = toList >>> \case
      [] -> S.empty
      (x : xs) -> foldl' S.intersection x xs

-- | Do no isolation, instead produce a single ambiguity covering the entirety of
-- the AST. This has terrible complexity, since ambiguities interact multiplicatively.
dummyIsolate :: (Dag tok, HashSet Node)
             -> Result (Seq (NodeOrElide tok)) (FullNode tok)
dummyIsolate (dag, roots) = runST $ do
  state <- State dag <$> newSTRef M.empty
  runReaderT (dummyAmb roots) state <&> \case
    (tree Seq.:<| Seq.Empty) -> Data tree
    trees -> Error $ mkElide <$> trees
  where
    mkElide :: FullNode tok -> NodeOrElide tok
    mkElide = project >>> fmap mkElide >>> Node

dummyAmb :: HashSet Node -> IsolationM s tok (Seq (FullNode tok))
dummyAmb = toList
  >>> traverse (\node -> fetchNode node >>= traverse dummyAmb <&> sequence <&> fmap embed)
  >>> fmap mconcat

fetchNode :: Node -> IsolationM s tok (NodeF tok (HashSet Node))
fetchNode node = asks
  $ dag
  >>> M.lookup node
  >>> compFromJust "P5DynamicAmbiguity.AmbiguityReporter2" ("Missing node " <> show node)
