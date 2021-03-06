module Data.Automaton.NFA
( NFA(..)
, addEdges
, merge
, destructiveMerge
, mergeTransitions
, isDeterministic
, renumber
, accessible
) where

import Pre

import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S

import Util (iterateInductively)

import Data.Automaton (FiniteAutomaton(..), EpsNFA(EpsNFA))

data NFA state alphabet = NFA
  { initial :: state
  , transitions :: HashMap state (HashMap alphabet (HashSet state))
  , final :: HashSet state }

isDeterministic :: NFA s a -> Bool
isDeterministic NFA{transitions} =
  all (all deterministic) transitions
  where
    deterministic s = S.size s <= 1

addEdges :: (Eq s, Hashable s, Eq a, Hashable a) => s -> a -> HashSet s -> NFA s a -> NFA s a
addEdges start t end a@NFA{transitions} = a
  { transitions = transitions & M.insertWith (M.unionWith S.union) start (M.singleton t end) }

merge :: (Eq s1, Hashable s1, Eq s2, Hashable s2) => NFA s1 a -> NFA s2 a -> NFA (Either s1 s2) a
merge a1 a2 = NFA
  { initial = Left initial
  , transitions = transform Left t1 ++ transform Right t2 & M.fromList
  , final = S.map Left f1 `S.union` S.map Right f2 }
  where
    NFA{initial, transitions = t1, final = f1} = a1
    NFA{transitions = t2, final = f2} = a2
    transform f = M.toList >>> fmap (f *** fmap (S.map f))

destructiveMerge :: (Eq s, Hashable s, Eq a, Hashable a) => NFA s a -> NFA s a -> NFA s a
destructiveMerge a1 a2 = NFA
  { initial = initial a1
  , transitions = mergeTransitions (transitions a1) (transitions a2)
  , final = final a1 <> final a2 }

mergeTransitions :: (Eq s, Hashable s, Eq a, Hashable a) => HashMap s (HashMap a (HashSet s)) -> HashMap s (HashMap a (HashSet s)) -> HashMap s (HashMap a (HashSet s))
mergeTransitions = M.unionWith (M.unionWith S.union)

renumber :: (Eq s, Hashable s) => NFA s a -> (NFA Int a, HashMap s Int)
renumber NFA{initial, transitions, final} = (, mapping) $ NFA
  { initial = convert initial
  , transitions = M.toList transitions
    & fmap (convert *** fmap (S.map convert))
    & M.fromList
  , final = S.map convert final }
  where
    getStates (s, tr) = S.insert s $ fold tr
    states = final `S.union` (M.toList transitions & foldMap getStates) & S.insert initial
    mapping = S.toList states `zip` [1..] & M.fromList
    convert s = M.lookup s mapping & compFromJust "Automaton.NFA.renumber" "missing state"

accessible :: (Eq s, Hashable s, Eq a, Hashable a) => NFA s a -> HashSet s
accessible NFA{initial, transitions} =
  iterateInductively transition (S.singleton initial)
  where
    transition s = M.lookupDefault mempty s transitions & fold

instance FiniteAutomaton NFA where
  asEpsNFA NFA{initial, transitions, final} = EpsNFA initial (convert <$> transitions) final
    where
      convert = M.toList >>> fmap (first Just) >>> M.fromList
  mapState convert NFA{initial, transitions, final} = NFA
    { initial = convert initial
    , transitions = M.toList transitions
      & fmap (convert *** fmap (S.map convert))
      & M.fromList
    , final = S.map convert final }
