module Automaton.EpsilonNFA
( edge
, addEdge
, mergeTransitions
, determinize
, states
, renumber
) where

import Pre

import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S

import Util (iterateInductivelyOptM, iterateInductively)

import Automaton (EpsNFA(..))
import Automaton.DFA (DFA(DFA))
import qualified Automaton.DFA as D

edge :: (Hashable s, Hashable a) => s -> Maybe a -> s -> HashMap s (HashMap (Maybe a) (HashSet s))
edge s a e = M.singleton s $ M.singleton a (S.singleton e)

addEdge :: (Eq s, Hashable s, Eq a, Hashable a) => s -> Maybe a -> s -> EpsNFA s a -> EpsNFA s a
addEdge start t end a@EpsNFA{transitions} = a
  { transitions = transitions & M.insertWith (M.unionWith S.union) start (M.singleton t (S.singleton end)) }

mergeTransitions :: (Eq s, Hashable s, Eq a, Hashable a) => HashMap s (HashMap (Maybe a) (HashSet s)) -> HashMap s (HashMap (Maybe a) (HashSet s)) -> HashMap s (HashMap (Maybe a) (HashSet s))
mergeTransitions = M.unionWith (M.unionWith S.union)

states :: forall s a. (Eq s, Hashable s) => EpsNFA s a -> HashSet s
states EpsNFA{initial, transitions, final} = S.insert initial $
  final `S.union` (M.toList transitions & foldMap getStates)
  where
    getStates (s, tr) = S.insert s $ fold tr

determinize :: forall s a. (Eq s, Hashable s, Eq a, Hashable a) => EpsNFA s a -> DFA (HashSet s) a
determinize EpsNFA{initial, transitions, final} = DFA
  { D.initial = startState
  , D.transitions = transitions'
  , D.final = S.filter (S.intersection final >>> S.null >>> not) newStates }
  where
    startState = epsClose $ S.singleton initial
    epsClose = iterateInductively (transition Nothing)
    transition tr s = M.lookup s transitions >>= M.lookup tr & fold

    (newStates, transitions') = runState
      (iterateInductivelyOptM findTransitions $ S.singleton startState)
      mempty

    findTransitions :: forall. HashSet s -> State (HashMap (HashSet s) (HashMap a (HashSet s))) (HashSet (HashSet s))
    findTransitions origins = S.toList origins
      & fmap (`M.lookup` transitions)
      & fmap fold
      & foldl' (M.unionWith S.union) M.empty
      & M.toList
      & mapMaybe removeEpsTransition
      & M.fromList
      & addTransitions origins
    removeEpsTransition (Nothing, _) = Nothing
    removeEpsTransition (Just t, s) = Just (t, epsClose s)

    addTransitions :: forall. HashSet s -> HashMap a (HashSet s) -> State (HashMap (HashSet s) (HashMap a (HashSet s))) (HashSet (HashSet s))
    addTransitions origin tr = do
      modify (M.insert origin tr)
      return $ S.fromList $ M.elems tr

renumber :: (Eq s, Hashable s) => EpsNFA s a -> (EpsNFA Int a, HashMap s Int)
renumber nfa@EpsNFA{initial, transitions, final} = (, mapping) $ EpsNFA
  { initial = convert initial
  , transitions = M.toList transitions
    & fmap (convert *** fmap (S.map convert))
    & M.fromList
  , final = S.map convert final }
  where
    mapping = S.toList (states nfa) `zip` [1..] & M.fromList
    convert s = M.lookup s mapping & compFromJust "Automaton.EpsNFA.renumber.convert" "missing state"
