module Automaton.DFA
( DFA(..)
, renumber
, minimize
, asNFA
) where

import Pre

import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S

import Automaton (FiniteAutomaton(..), EpsNFA(EpsNFA))
import qualified Automaton.NFA as N

import Util (flipMap)

data DFA state alphabet = DFA
  { initial :: state
  , transitions :: HashMap state (HashMap alphabet state)
  , final :: HashSet state }

renumber :: (Eq s, Hashable s) => DFA s a -> (DFA Int a, HashMap s Int)
renumber dfa@DFA{initial, transitions, final} = (, mapping) $ DFA
  { initial = convert initial
  , transitions = M.toList transitions
    & fmap (convert *** fmap convert)
    & M.fromList
  , final = S.map convert final }
  where
    mapping = S.toList (states dfa) `zip` [1..] & M.fromList
    convert s = M.lookup s mapping & compFromJust "Automaton.DFA.renumber.convert" "missing state"

states :: (Eq s, Hashable s) => DFA s a -> HashSet s
states DFA{initial, transitions, final} = S.insert initial $
  final `S.union` (M.toList transitions & foldMap getStates)
  where
    getStates (s, tr) = S.insert s $ foldMap S.singleton tr

data MinimizationState s = MS
  { partitions :: HashSet (HashSet s)
  , workingSet :: HashSet (HashSet s) }

minimize :: forall s a. (Eq s, Hashable s, Eq a, Hashable a) => DFA s a -> DFA (HashSet s) a
minimize dfa@DFA{initial, transitions, final} = DFA
  { initial = convert initial
  , transitions = M.toList transitions & fmap (convert *** fmap convert) & M.fromList
  , final = S.map convert final }
  where
    convert :: s -> HashSet s
    convert s = M.lookup s translationMap
      & compFromJust "Automaton.DFA.minimize.convert" "missing state"
    translationMap = S.toList partitions
      & fmap (\p -> S.toList p `zip` repeat p & M.fromList)
      & M.unions
    MS{partitions} = execState computePartitions MS
      { partitions = S.fromList [final, states dfa `S.difference` final]
      , workingSet = S.singleton final }
    computePartitions =
      repeatWhile (gets $ workingSet >>> S.toList) $ \w -> do
        modify $ \ms@MS{workingSet} -> ms { workingSet = S.delete w workingSet }
        let predecessors = S.toList w
              & fmap (`M.lookup` revTransitions)
              & fmap fold
              & foldl' (M.unionWith S.union) mempty
        forM_ predecessors $ \predSet -> do
          MS{partitions = partitionSets} <- get
          forM_ partitionSets $ \partSet -> do
            let shared = S.intersection predSet partSet
                nonShared = S.difference partSet predSet
            unless (S.null shared || S.null nonShared) $ do
              modifyPartitions $ S.delete partSet >>> S.insert shared >>> S.insert nonShared
              gets (workingSet >>> S.member partSet) >>= \case
                True ->
                  modifyWorkingSet $ S.delete partSet >>> S.insert shared >>> S.insert nonShared
                False ->
                  modifyWorkingSet $ S.insert (if S.size shared < S.size nonShared then shared else nonShared)
    revTransitions :: HashMap s (HashMap a (HashSet s))
    revTransitions = flipMap transitions

    modifyPartitions f = modify $ \ms@MS{partitions = p} -> ms { partitions = f p }
    modifyWorkingSet f = modify $ \ms@MS{workingSet = w} -> ms { workingSet = f w }

repeatWhile :: Monad m => m [a] -> (a -> m b) -> m ()
repeatWhile mlist action = mlist >>= \case
  a : _ -> action a >> repeatWhile mlist action
  [] -> return ()

instance FiniteAutomaton DFA where
  asEpsNFA DFA{initial, transitions, final} = EpsNFA initial (convert <$> transitions) final
    where
      convert = M.toList >>> fmap (Just *** S.singleton) >>> M.fromList
  mapState convert DFA{initial, transitions, final} = DFA
    { initial = convert initial
    , transitions = M.toList transitions
      & fmap (convert *** fmap convert)
      & M.fromList
    , final = S.map convert final }

asNFA :: (Hashable s) => DFA s a -> N.NFA s a
asNFA DFA{initial, transitions, final} = N.NFA initial (fmap S.singleton <$> transitions) final
