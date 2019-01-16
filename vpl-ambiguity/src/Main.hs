module Main where

import Pre

import Data.String (fromString)

import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S

import qualified Regex as R
import qualified Automaton as FA
import qualified Automaton.NFA as N
import qualified Automaton.DFA as D
import qualified Automaton.EpsilonNFA as E
import qualified GraphViz as GV

-- TODO: ensure that the "out" directory exists
-- TODO: actually run dot on the generated graphs
main :: IO ()
main = do
  makeAndOutputParenNFA exprGrammar "ExprGrammar"

-- Terminal, NonTerminal, LeftParen, RightParen
data Alphabet t nt = T t | NT nt | L | R deriving (Show, Eq, Generic)
instance (Hashable t, Hashable nt) => Hashable (Alphabet t nt)

type Language = HashMap Text (R.Regex (Alphabet Text Text))

instance IsString t => IsString (Alphabet t nt) where
  fromString = fromString >>> T

-- returns (the automaton, a mapping from non-terminals to their final states)
toParenNFA :: Language -> (N.NFA Int (Alphabet Text Text), HashMap Text Int)
toParenNFA language = (composedNFA, M.mapWithKey finalTranslation language)
  where
    nfas = M.mapWithKey toMinimalDFA language
    toMinimalDFA k = R.toAutomaton >>> E.determinize >>> D.minimize
      >>> D.renumber >>> fst >>> FA.mapState (k,) >>> D.asNFA

    initState = ("toParenNFA", -1)
    finalState = (, -1)  -- NOTE: we here assume that no NFA uses negative numbers before this, so that this state is distinct from all the others
    allFinals = finalState <$> M.keys language & S.fromList

    transitionsFromInit = M.singleton initState $
      M.singleton L (foldMap (N.initial >>> S.singleton) nfas)
    finalTransitionsForNT (nt, N.NFA{N.final}) =
      S.toList final `zip` repeat (M.singleton R (S.singleton $ finalState nt))
        & M.fromList
    allTransitions = N.transitions <$> M.elems nfas
      & mappend (finalTransitionsForNT <$> M.toList nfas)
      & foldl N.mergeTransitions M.empty
      & N.mergeTransitions transitionsFromInit

    (composedNFA, rawTranslation) = N.renumber $ N.NFA
      { N.initial = initState
      , N.transitions = allTransitions
      , N.final = allFinals }
    finalTranslation nt _ = M.lookup (finalState nt) rawTranslation
      & compFromJust "Main.toParenNFA.finalTranslation" ("missing final state for " <> show nt)

makeAndOutputParenNFA :: Language -> FilePath -> IO ()
makeAndOutputParenNFA lang path = do
  let (nfa, ntToS) = toParenNFA lang
      sToNt = M.toList ntToS & fmap swap & M.fromList
      sToStr s = M.lookupDefault "" s sToNt
  writeFile ("out/" <> path <> ".dot") $ GV.toDotText sToStr show nfa

listRegex :: R.Regex (Alphabet Text Text)
listRegex = R.concat ["[", R.opt $ R.Concat e (R.star $ R.Concat ";" e), "]"]
  where
    e = R.Terminal $ NT "E"

exprGrammar :: Language
exprGrammar = M.fromList
  [("E",
    R.choice
     [ R.concat [e, ";", e]
     , R.concat ["[", R.opt $ R.Concat e (R.star $ R.Concat ";" e), "]"]
     , "a"])]
  where
    e = R.Terminal $ NT "E"
