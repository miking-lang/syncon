{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell, RecursiveDo #-} -- TODO: remove this

-- module P4Parsing.ForestParser where
module P4Parsing.ForestParser
( Prod
, Grammar
, rule
, terminal
, ambig
, alts
, parse
, Node
, forestToDot
, test
) where

import Pre hiding (from)
import Text.Show.Pretty (pPrint)

import qualified Data.Text as Text
import qualified Data.HashMap.Lazy as M

import P4Parsing.ForestParser.Grammar (Prod, Grammar, rule, alts, terminal, ambig)
import P4Parsing.ForestParser.GLL (parse, Node)

import Data.Functor.Foldable.TH (makeBaseFunctor)

data Expr = Var Text
          | Plus Expr Expr
          | ListE [Expr]
makeBaseFunctor ''Expr
deriving instance Show a => Show (ExprF a)

test :: IO ()
test =
  case parse exprGrammar (input :: [Text]) of
    Left err -> putStrLn err
    Right a -> do
      pPrint a
      putStrLn $ forestToDot show a
  where
    -- input = ["[", "a", "+", "b", "+", "c", "]"]
    input = "[(a+b+c)+d+e,fg]" <&> Text.singleton

exprGrammar :: forall r. Grammar r Text ExprF (Prod r Text ExprF r)
exprGrammar = mdo
  var <- rule $ VarF <$> terminal "Var" isVar
  plus <- rule $ PlusF <$> expr <* terminal "+" (== "+") <*> expr
  let sepBy :: Prod r Text ExprF a -> Prod r Text ExprF b -> Prod r Text ExprF [a]
      sepBy e sep = (:) <$> e <*> many (sep *> e) & optional <&> fold
  list <- rule $ terminal "[" (== "[") *> (ListEF <$> sepBy expr (terminal "," (== ","))) <* terminal "]" (== "]")
  let paren = terminal "(" (== "(") *> expr <* terminal ")" (== ")")
  expr <- ambig [var, plus, list, paren]
  return expr

  where
    isVar "+" = False
    isVar "(" = False
    isVar ")" = False
    isVar "[" = False
    isVar "]" = False
    isVar "," = False
    isVar _ = True

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

    newAlt :: State Int (Int, Text)
    newAlt = do
      id <- get <* modify (+1)
      return (id, "  " <> show id <> " [shape=point, label=\"\"];\n")

    nodeEdgeDescs = evalState (foldMapM nodeEdges $ M.toList nodeMap) firstAmbigNode

    nodeEdges :: (n, nodeF (HashSet n)) -> State Int Text
    nodeEdges (keyToI -> from, node) = flip foldMapM node $ \alternatives -> do
      (altId, altDesc) <- newAlt
      let dests = toList alternatives <&> keyToI
      return
        $ altDesc
        <> "  " <> show from <> " -> " <> show altId <> ";\n"
        <> "  " <> show altId <> " -> {" <> (dests <&> show & intersperse ", " & Text.concat) <> "};\n"

escape :: Text -> Text
escape = Text.concatMap f
  where
    f '\\' = "\\\\"
    f '"' = "\\\""
    f c = Text.singleton c
