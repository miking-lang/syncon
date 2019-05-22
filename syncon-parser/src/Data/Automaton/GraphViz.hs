module Data.Automaton.GraphViz where

import Pre

import qualified Data.Text as Text

import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import System.IO.Unsafe (unsafePerformIO)

import Data.Automaton (EpsNFA(..), FiniteAutomaton(..))
import qualified Data.Automaton.EpsilonNFA as E

-- TODO: escape labels
toDotText :: (Eq s, Hashable s, Eq a, Hashable a, FiniteAutomaton fa)
          => (s -> Text) -> (a -> Text) -> fa s a -> Text
toDotText sToStrOriginal aToStrOriginal originalFA = "digraph {\n"
  <> "  rankdir=LR;\n"
  <> "  startState [shape=point, label=\"\"];\n"
  <> "  startState -> " <> show initial <> ";\n"
  <> foldMap stateDescription (E.states nfa)
  <> foldMap edgeDescription (M.toList transitions)
  <> "}\n"
  where
    aToStr = aToStrOriginal >>> escape
    (nfa@EpsNFA{initial, transitions, final}, sToInt) = E.renumber $ asEpsNFA originalFA
    intToS = M.toList sToInt & fmap swap & M.fromList
    sToStr s = M.lookup s intToS
      & compFromJust "GraphViz.toDot.sToStr" "unknown int state"
      & sToStrOriginal
      & escape

    stateDescription s =
      "  " <> show s <> " [label = \"" <> sToStr s <> "\", shape=" <> shape s <> "];\n"
    shape s
      | s `S.member` final = "doublecircle"
      | otherwise = "circle"

    edgeDescription (s, outgoing) = foldMap edge (M.toList outgoing)
      where
        edge (a, ss) = "  " <> show s <> " -> {" <> (S.toList ss & fmap show & intersperse ", " & mconcat)
          <> "} [label=\"" <> maybe "eps" aToStr a <> "\"];\n"

writeDotFile :: (Eq s, Hashable s, Eq a, Hashable a, FiniteAutomaton fa)
          => FilePath -> (s -> Text) -> (a -> Text) -> fa s a -> IO ()
writeDotFile path sToStr aToStr fa = do
  writeFile path (toDotText sToStr aToStr fa)

debugWriteDotFile :: (Eq s, Hashable s, Eq a, Hashable a, FiniteAutomaton fa) => FilePath -> (s -> Text) -> (a -> Text) -> fa s a -> b -> b
debugWriteDotFile path sToStr aToStr fa b = unsafePerformIO $ do
  writeDotFile path sToStr aToStr fa
  return b

escape :: Text -> Text
escape = Text.concatMap f
  where
    f '\\' = "\\\\"
    f '"' = "\\\""
    f c = Text.singleton c
