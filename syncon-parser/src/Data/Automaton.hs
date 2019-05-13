module Data.Automaton where

import Pre

import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S

data EpsNFA state alphabet = EpsNFA
  { initial :: state
  , transitions :: HashMap state (HashMap (Maybe alphabet) (HashSet state))
  , final :: HashSet state }

class FiniteAutomaton fa where
  asEpsNFA :: (Eq s, Hashable s, Eq a, Hashable a) => fa s a -> EpsNFA s a
  mapState :: (Eq s2, Hashable s2) => (s1 -> s2) -> fa s1 a -> fa s2 a

instance FiniteAutomaton EpsNFA where
  asEpsNFA = identity
  mapState convert EpsNFA{initial, transitions, final} = EpsNFA
    { initial = convert initial
    , transitions = M.toList transitions
      & fmap (convert *** fmap (S.map convert))
      & M.fromList
    , final = S.map convert final }
