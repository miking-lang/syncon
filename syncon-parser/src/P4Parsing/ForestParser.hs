{-# LANGUAGE ViewPatterns #-}

-- module P4Parsing.ForestParser where
module P4Parsing.ForestParser
( Prod
, Grammar
, rule
, ranged
, terminal
, ambig
, alts
, parse
, Node
, forestToDot
, Unlexable(..)
) where

import Pre hiding (from)

import qualified Data.Text as Text
import qualified Data.HashMap.Lazy as M

import P4Parsing.ForestParser.Grammar (Prod, Grammar, rule, alts, terminal, ambig, ranged, Unlexable(..))
import P4Parsing.ForestParser.GLL (parse, Node)

forestToDot :: forall nodeF n. (Foldable nodeF, Functor nodeF, Eq n, Hashable n)
            => (nodeF () -> Text) -> (HashMap n (nodeF (HashSet n)), HashSet n) -> Text
forestToDot showNode (nodeMap, roots) = "digraph {\n"
  <> "  startState [shape=point, label=\"\"];\n"
  <> foldMap (\n -> "  startState -> " <> show (keyToI n) <> ";\n") roots
  <> foldMap nodeDesc (M.toList nodeMap)
  <> nodeEdgeDescs
  <> "}\n"
  where
    keyToIMap = M.keys nodeMap `zip` [(0::Int)..] & M.fromList
    keyToI key = M.lookup key keyToIMap & compFromJust "P4Parsing.ForestParser.forestToDot" "Missing key in keyToIMap"
    nodeDesc (key, node) = "  " <> show (keyToI key) <> " [label = \"" <> escape (showNode $ void node) <> "\"];\n"
    firstAmbigNode = maximum keyToIMap + 1

    genAlt :: HashSet n -> State (HashMap (HashSet n) Int, Int) (Int, Text)
    genAlt alt = do
      (prevs, id) <- get
      case M.lookup alt prevs of
        Just prevId -> return (prevId, "")
        Nothing -> do
          let altDesc = "  " <> show id <> " [shape=point, label=\"\"];\n"
              edges = "  " <> show id <> " -> {" <> (toList alt <&> keyToI <&> show & intersperse ", " & Text.concat) <> "};\n"
          modify $ M.insert alt id *** (+1)
          return (id, altDesc <> edges)

    nodeEdgeDescs = evalState (foldMapM nodeEdges $ M.toList nodeMap) (M.empty, firstAmbigNode)

    nodeEdges :: (n, nodeF (HashSet n)) -> State (HashMap (HashSet n) Int, Int) Text
    nodeEdges (keyToI -> from, node) = flip foldMapM node $ \alternatives -> do
      (altId, altDesc) <- genAlt alternatives
      return $ altDesc
        <> "  " <> show from <> " -> " <> show altId <> "[arrowhead=none];\n"

escape :: Text -> Text
escape = Text.concatMap f
  where
    f '\\' = "\\\\"
    f '"' = "\\\""
    f c = Text.singleton c
