{-# OPTIONS_GHC -fno-warn-unused-imports #-}  -- TODO: remove this

module P5DynamicAmbiguity.AmbiguityReporter2 (isolate) where

import Pre hiding (reduce, State, state, orElse)
import Result (Result(..))

import Text.Printf (printf)
import qualified Data.Text as Text
import qualified Data.HashSet as S
import qualified Data.HashMap.Lazy as M
import qualified Data.Sequence as Seq
import Data.Semigroup (Max(..))
import Data.STRef (STRef, newSTRef, readSTRef, modifySTRef')

import Data.Functor.Foldable (embed)

import P2LanguageDefinition.Types (TypeName(..))
import P4Parsing.ForestParser (Node)
import P4Parsing.Types (SingleLanguage)
import qualified P4Parsing.Types as P4
import P5DynamicAmbiguity.Types hiding (NodeOrElide)
import qualified P5DynamicAmbiguity.Types as P5

-- TODO: I suspect that only (HashSet Node) will be chosen for elison, check this
type Elidable = Either Node (HashSet Node)
type NodeOrElide = P5.NodeOrElide Elidable

type NodeF = P4.NodeF SingleLanguage TypeName

type Dag = HashMap Node (NodeF (HashSet Node))

data State s = State
  { dag :: !Dag
  , validElisonsMemo :: !(STRef s (HashMap Elidable (HashSet Elidable)))  -- ^ Memoized results of the elidable analysis
  }
type IsolationM s a = ReaderT (State s) (ST s) a

type FullNode = P4.Node SingleLanguage TypeName
type Res = Result (HashMap (HashSet Node) (Seq NodeOrElide))

-- | Produce a single parse tree, or a disjoint set of minimal ambiguities.
isolate :: (Dag, HashSet Node)
        -> Result (Seq (Seq NodeOrElide)) FullNode
isolate (dag, roots) = runST $ do
  state <- State dag <$> newSTRef M.empty
  runReaderT (amb roots) state <&> first (toList >>> Seq.fromList)

-- | Given a potential ambiguity, construct either a complete subtree, or a disjoint set of
-- minimal ambiguities
amb :: HashSet Node -> IsolationM s (Res FullNode)
amb nodes
  | [node] <- toList nodes = do
      fetchNode node >>= traverse amb <&> sequence <&> fmap embed
  | otherwise = do
      elidables <- validElidables $ Right nodes
      mkAmb elidables nodes <&> Error

-- | Construct the ambiguity rooted at the second argument, using the first argument as the
-- nodes that are valid elisons.
mkAmb :: forall s. HashSet Elidable -> HashSet Node -> IsolationM s (HashMap (HashSet Node) (Seq NodeOrElide))
mkAmb elidable roots = recurAmb roots
  <&> second (Seq.fromList >>> M.singleton roots >>> Error)
  <&> uncurry (<*)
  <&> fromError
  where
    -- recur and recurAmb are mutually recursive and perform the traversal that constructs the
    -- delimited parse trees that are a part of the current ambiguity. That's the second value
    -- in the returned tuple. The first value is a computation that has looked for ambiguities
    -- further down the AST.
    recurAmb :: HashSet Node -> IsolationM s (Res (), [NodeOrElide])
    recurAmb nodes
      | Right nodes `S.member` elidable = amb nodes <&> void <&> (,[Elide $ Right nodes])
      | otherwise = toList nodes & traverse recur <&> mconcat
    recur :: Node -> IsolationM s (Res (), [NodeOrElide])
    recur node
      | Left node `S.member` elidable = amb (S.singleton node) <&> void <&> (,[Elide $ Left node])
      | otherwise = fetchNode node >>= traverse recurAmb <&> sequence <&> second (sequence >>> fmap Node)

    fromError (Error e) = e
    fromError Data{} = compErr "P5DynamicAmbiguity.AmbiguityReporter2.mkAmb" "impossible"

-- | Given a particular root (most commonly an ambiguity node HashSet Node), find the set of nodes
-- that can be elided, i.e., the nodes that are present in all alternatives.
validElidables :: Elidable -> IsolationM s (HashSet Elidable)
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

fetchNode :: Node -> IsolationM s (NodeF (HashSet Node))
fetchNode node = asks
  $ dag
  >>> M.lookup node
  >>> compFromJust "P5DynamicAmbiguity.AmbiguityReporter2" ("Missing node " <> show node)
