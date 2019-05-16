{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Automaton.DVA where

import Pre hiding (reverse)

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S

import Data.Align (align, padZip)
import Data.These (fromThese)

import Util (iterateInductivelyOptM, repeatUntilStable)

import Data.Automaton.NVA (NVA(NVA))
import qualified Data.Automaton.NVA as NVA

data DVA s sta i o c = DVA
  { initial :: !s
  , innerTransitions :: !(HashMap s (HashMap i s))
  , openTransitions :: !(HashMap s (HashMap o (sta, s)))
  , closeTransitions :: !(HashMap s (HashMap c (HashMap sta s)))
  , final :: !(HashSet s) }

data DeterminizeState s sta i o c = DeterminizeState
  { inners :: HashMap s (HashMap i s)
  , opens :: HashMap s (HashMap o (sta, s))
  , closes :: HashMap s (HashMap c (HashMap sta s)) }

instance
  ( Eq s, Hashable s
  , Eq sta, Hashable sta
  , Eq i, Hashable i
  , Eq o, Hashable o
  , Eq c, Hashable c )
  => Semigroup (DeterminizeState s sta i o c) where
  DeterminizeState i1 o1 c1 <> DeterminizeState i2 o2 c2 = DeterminizeState
    (M.unionWith M.union i1 i2)
    (M.unionWith M.union o1 o2)
    (M.unionWith (M.unionWith M.union) c1 c2)

instance
  ( Eq s, Hashable s
  , Eq sta, Hashable sta
  , Eq i, Hashable i
  , Eq o, Hashable o
  , Eq c, Hashable c )
  => Monoid (DeterminizeState s sta i o c) where
  mappend = (<>)
  mempty = DeterminizeState mempty mempty mempty

dstates :: (Eq s, Hashable s) => DeterminizeState s sta i o c -> HashSet s
dstates DeterminizeState{..} = S.fromList $ is <> os <> cs
  where
    is = M.keys inners <> (toList inners >>= toList)
    os = M.keys opens <> (toList opens >>= fmap snd . toList)
    cs = M.keys closes <> (toList closes >>= toList >>= toList)

stackSyms :: (Eq sta, Hashable sta) => DeterminizeState s sta i o c -> HashSet sta
stackSyms DeterminizeState{opens, closes} = S.fromList $ os <> cs
  where
    os = toList opens >>= fmap fst . toList
    cs = toList closes >>= toList >>= M.keys

-- WARNING: the state type of the resulting DVA is of type HashMap s (HashSet s), where the SECOND s is the origin state, and the FIRST edge is the destination edge, i.e., they're swapped from the literature
determinize :: forall s sta i o c.
               ( Eq s, Hashable s
               , Eq sta, Hashable sta
               , Eq i, Hashable i
               , Eq o, Hashable o
               , Eq c, Hashable c )
            => NVA s sta i o c -> DVA (HashMap s (HashSet s)) (HashMap s (HashSet s), o) i o c
determinize NVA
  { NVA.initial = ini
  , NVA.innerTransitions = it
  , NVA.openTransitions = ot
  , NVA.closeTransitions = ct
  , NVA.final = fin } = DVA
  { initial = startState
  , innerTransitions = inners allEdges
  , openTransitions = opens allEdges
  , closeTransitions = closes allEdges
  , final = S.empty }
  & adjustFinal
  where
    startState = idQ ini
    adjustFinal dva = dva
      { final = S.filter (M.keys >>> S.fromList >>> S.intersection fin >>> S.null >>> not) (states dva) }

    allEdges = execState discoverAll mempty
    discoverAll = discoverOnce >> gets stackSyms & repeatUntilStable
    discoverOnce = gets dstates >>= \currStates -> iterateInductivelyOptM discoverSuccessors
      (if S.null currStates
      then S.singleton startState
      else currStates)

    ctMap :: HashMap s (HashMap c (HashMap sta (HashSet s)))
    ctMap = fmap (fmap $ toList >>> fmap (second S.singleton) >>> M.fromListWith S.union) ct

    otMap :: HashMap s (HashMap o (HashMap sta (HashSet s)))
    otMap = fmap (fmap $ toList >>> fmap (second S.singleton) >>> M.fromListWith S.union) ot

    discoverSuccessors invBotTrans = do
      let is = M.intersectionWith (\origins outEdges -> cartesianProduct origins <$> outEdges) invBotTrans it
               & foldl' (M.unionWith S.union) M.empty
               & fmap toStateRep
          os = M.intersection ot invBotTrans
               & foldl' (M.unionWith S.union) M.empty
               & M.mapWithKey (\o stas -> ((invBotTrans, o), (idQ $ S.map snd stas)))

      addTransitions $ DeterminizeState
        { inners = M.singleton invBotTrans is
        , opens = M.singleton invBotTrans os
        , closes = M.empty }

      staSyms <- gets stackSyms
      cEnds <- forM (toList staSyms) $ \staSym@(invBotTrans', o) -> do
        -- NOTE: using names from the optimized version by Van Tang, arrows are HashMaps
            -- haveOpen :: q1 -> gamma -> set q
        let haveOpen = M.intersectionWith (,) invBotTrans' (M.mapMaybe (M.lookup o) otMap)
                       & foldl' (\a b -> M.unionWith (M.unionWith S.union) a (shuffleOpenEdges b)) M.empty
            -- invBotTransOpen :: q2 -> gamma -> set q
            invBotTransOpen = invBotTrans
              & fmap (S.toMap >>> M.intersection haveOpen >>> foldl' (M.unionWith S.union) M.empty)
              & M.filter (M.null >>> not)
            -- origins :: gamma -> set q
            -- destinations :: a -> gamma -> set q'
            cs = M.intersectionWith
              (\origins destinations -> M.intersectionWith cartesianProduct origins <$> destinations)
              invBotTransOpen
              ctMap
              & foldl' (M.unionWith $ M.unionWith S.union) M.empty
              & fmap (fold >>> toStateRep >>> M.singleton staSym)
        addTransitions $ DeterminizeState
          { inners = M.empty
          , opens = M.empty
          , closes = M.singleton invBotTrans cs }
        return $ toList cs >>= toList

      return $ S.fromList $ concat cEnds <> (toList os <&> snd) <> toList is

    idQ :: HashSet s -> HashMap s (HashSet s)
    idQ = S.toMap >>> M.mapWithKey (\s _ -> S.singleton s)

    toStateRep :: HashSet (s, s) -> HashMap s (HashSet s)
    toStateRep = toList >>> fmap (swap >>> second S.singleton) >>> M.fromListWith S.union

    addTransitions = mappend >>> modify

    -- WARNING: this makes a map from destination state to origin state
    shuffleOpenEdges :: (HashSet s, HashMap sta (HashSet s)) -> HashMap s (HashMap sta (HashSet s))
    shuffleOpenEdges (origins, outEdges) = M.fromListWith (M.unionWith S.union) $ do
      (sta, destinations) <- M.toList outEdges
      destination <- toList destinations
      return (destination, M.singleton sta origins)

    cartesianProduct :: (Eq a, Hashable a, Eq b, Hashable b)
                     => HashSet a -> HashSet b -> HashSet (a, b)
    cartesianProduct as bs = (,) <$> S.toList as <*> S.toList bs & S.fromList

reverse :: ( Eq s, Hashable s
           , Eq sta, Hashable sta
           , Eq i, Hashable i
           , Eq o, Hashable o
           , Eq c, Hashable c )
        => DVA s sta i o c -> NVA s sta i c o
reverse DVA{..} = NVA
  { NVA.initial = final
  , NVA.final = S.singleton initial
  , NVA.innerTransitions = innerTransitions
    <&> fmap S.singleton
    & NVA.toTriples
    <&> (\(a, b, c) -> (c, b, a))
    & NVA.fromTriples
  , NVA.openTransitions = closeTransitions
    <&> fmap (M.toList >>> S.fromList)
    & NVA.toTriples
    <&> (\(a, b, (sta, c)) -> (c, b, (sta, a)))
    & NVA.fromTriples
  , NVA.closeTransitions = openTransitions
    <&> fmap S.singleton
    & NVA.toTriples
    <&> (\(a, b, (sta, c)) -> (c, b, (sta, a)))
    & NVA.fromTriples }

-- TODO: BUG: The trimming paper suggests that this should work (more or less anyway), but it doesn't, look into why
determinizeAndTrim :: ( Eq s, Hashable s
                      , Eq sta, Hashable sta
                      , Eq i, Hashable i
                      , Eq o, Hashable o
                      , Eq c, Hashable c )
                   => NVA s sta i o c -> DVA Int Int i o c
determinizeAndTrim =
  NVA.reduce >>> determinize >>> reverse >>> NVA.reduce >>> NVA.trivialCoReduce >>> NVA.reverse >>> asDVA >>> \case
    Nothing -> compErr "Data.Automaton.DVA.determinizeAndTrim" "reduce did not preserve co-determinism"
    Just dva -> renumber dva

asDVA :: forall s sta i o c.
         (Eq s, Hashable s, Eq sta, Hashable sta)
      => NVA s sta i o c -> Maybe (DVA s sta i o c)
asDVA nva = do
  initial <- setToSingle $ NVA.initial nva
  innerTransitions <- traverse (traverse setToSingle) $ NVA.innerTransitions nva
  openTransitions <- traverse (traverse setToSingle) $ NVA.openTransitions nva
  closeTransitions <- traverse (traverse $ juggle >>> traverse setToSingle) $ NVA.closeTransitions nva
  let final = NVA.final nva
  return DVA{..}
  where
    juggle :: HashSet (sta, s) -> HashMap sta (HashSet s)
    juggle = toList >>> fmap (second S.singleton) >>> M.fromListWith S.union
    setToSingle :: HashSet a -> Maybe a
    setToSingle = toList >>> \case
      [a] -> Just a
      _ -> Nothing

states :: (Eq s, Hashable s) => DVA s sta i o c -> HashSet s
states DVA{..} = final <> S.fromList (is <> os <> cs)
  where
    is = M.keys innerTransitions <> (toList innerTransitions >>= toList)
    os = M.keys openTransitions <> (toList openTransitions >>= fmap snd . toList)
    cs = M.keys closeTransitions <> (toList closeTransitions >>= toList >>= toList)

mapStates :: (Eq s2, Hashable s2) => (s1 -> s2) -> DVA s1 sta i o c -> DVA s2 sta i o c
mapStates convert DVA{..} = DVA
  { initial = convert initial
  , final = S.map convert final
  , openTransitions = mapKeys convert openTransitions
    <&> fmap (second convert)
  , innerTransitions = mapKeys convert innerTransitions
    <&> fmap convert
  , closeTransitions = mapKeys convert closeTransitions
    <&> fmap (fmap convert) }

stackSymbols :: (Eq sta, Hashable sta) => DVA s sta i o c -> HashSet sta
stackSymbols DVA{openTransitions,closeTransitions} =
  (toList openTransitions >>= toList <&> fst & S.fromList)
  <> (toList closeTransitions >>= toList >>= M.keys & S.fromList)

mapSta :: (Eq sta2, Hashable sta2) => (sta1 -> sta2) -> DVA s sta1 i o c -> DVA s sta2 i o c
mapSta convert dva@DVA{openTransitions,closeTransitions} = dva
  { openTransitions = openTransitions <&> fmap (first convert)
  , closeTransitions = closeTransitions <&> fmap (mapKeys convert) }

-- TODO: this assumes that the transitions of the operands are total, but determinize above does not produce total transitions. Make this work without them (add a dummy fail state to each automaton, use it when an edge is missing)
dvaOp :: ( Eq s1, Hashable s1
         , Eq s2, Hashable s2
         , Eq sta1, Hashable sta1
         , Eq sta2, Hashable sta2
         , Eq i, Hashable i
         , Eq o, Hashable o
         , Eq c, Hashable c )
      => (Bool -> Bool -> Bool) -> DVA s1 sta1 i o c -> DVA s2 sta2 i o c -> DVA (Maybe s1, Maybe s2) (Maybe sta1, Maybe sta2) i o c
dvaOp op dva1 dva2 = DVA
  { initial = newInitial
  , innerTransitions = inners foundTransitions
  , openTransitions = opens foundTransitions
  , closeTransitions = closes foundTransitions
  , final = S.empty }
  & adjustFinal
  where
    newInitial = (Just $ initial dva1, Just $ initial dva2)
    adjustFinal dva = dva
      { final = states dva
        & S.filter (\(s1, s2) -> maybe False (`S.member` final dva1) s1 `op` maybe False (`S.member` final dva2) s2) }

    foundTransitions = execState
      (iterateInductivelyOptM findTransitions $ S.singleton newInitial)
      mempty

    dva1close = stackSymbols dva1 & S.map Just & S.toMap & fmap (const Nothing)
    dva2close = stackSymbols dva2 & S.map Just & S.toMap & fmap (const Nothing)

    findTransitions (s1, s2) = do
      let it1 = s1 >>= (`M.lookup` innerTransitions dva1) & fromMaybe M.empty
          it2 = s2 >>= (`M.lookup` innerTransitions dva2) & fromMaybe M.empty
          ot1 = s1 >>= (`M.lookup` openTransitions dva1) & fromMaybe M.empty
          ot2 = s2 >>= (`M.lookup` openTransitions dva2) & fromMaybe M.empty
          ct1 = s1 >>= (`M.lookup` closeTransitions dva1) & fromMaybe M.empty
          ct2 = s2 >>= (`M.lookup` closeTransitions dva2) & fromMaybe M.empty
      let is = padZip it1 it2
          os = align ot1 ot2
            <&> bimap (Just *** Just) (Just *** Just)
            <&> fromThese (Nothing, Nothing) (Nothing, Nothing)
            <&> (\((sta1, d1), (sta2, d2)) -> ((sta1, sta2), (d1, d2)))
          cs = align ct1 ct2
            <&> bimap (mapKeys Just >>> fmap Just >>> flip M.union dva1close) (mapKeys Just >>> fmap Just >>> flip M.union dva2close)
            <&> fromThese dva1close dva2close
            <&> uncurry cartesianProduct'
          newStates = S.fromList (toList is)
            <> S.fromList (toList os <&> snd)
            <> S.fromList (toList cs >>= toList)

      addTransitions $ DeterminizeState
        { inners = M.singleton (s1, s2) is
        , opens = M.singleton (s1, s2) os
        , closes = M.singleton (s1, s2) cs }

      return newStates

    addTransitions transitions = modify $ mappend transitions

    cartesianProduct' :: (Eq a, Hashable a, Eq c, Hashable c)
                      => HashMap a b -> HashMap c d -> HashMap (a, c) (b, d)
    cartesianProduct' ab cd = (\(a, b) (c, d) -> ((a, c), (b, d))) <$> M.toList ab <*> M.toList cd
      & M.fromList

product :: ( Eq s1, Hashable s1
           , Eq s2, Hashable s2
           , Eq sta1, Hashable sta1
           , Eq sta2, Hashable sta2
           , Eq i, Hashable i
           , Eq o, Hashable o
           , Eq c, Hashable c )
        => DVA s1 sta1 i o c -> DVA s2 sta2 i o c -> DVA (Maybe s1, Maybe s2) (Maybe sta1, Maybe sta2) i o c
product = dvaOp (&&)

difference :: ( Eq s1, Hashable s1
              , Eq s2, Hashable s2
              , Eq sta1, Hashable sta1
              , Eq sta2, Hashable sta2
              , Eq i, Hashable i
              , Eq o, Hashable o
              , Eq c, Hashable c )
           => DVA s1 sta1 i o c -> DVA s2 sta2 i o c -> DVA (Maybe s1, Maybe s2) (Maybe sta1, Maybe sta2) i o c
difference = dvaOp (\a b -> a && not b)

renumberStates :: (Eq s, Hashable s) => DVA s sta i o c -> DVA Int sta i o c
renumberStates dva = mapStates oldToNew dva
  where
    oldToNew s = M.lookup s newStates
      & compFromJust "Data.Automaton.DVA.renumberStates" "missing state"
    newStates = evalState computeNewStates 0
    computeNewStates = states dva & S.toMap & traverse (\_ -> get <* modify (+1))

renumberStack :: (Eq sta, Hashable sta) => DVA s sta i o c -> DVA s Int i o c
renumberStack dva = mapSta oldToNew dva
  where
    oldToNew sta = M.lookup sta newSymbols
      & compFromJust "Data.Automaton.EpsilonDVA.renumberStack" "missing symbol"
    newSymbols = evalState computeNewSymbols 0
    computeNewSymbols = stackSymbols dva & S.toMap & traverse (\_ -> get <* modify (+1))

renumber :: (Eq s, Hashable s, Eq sta, Hashable sta) => DVA s sta i o c -> DVA Int Int i o c
renumber = renumberStates >>> renumberStack

mapKeys :: (Eq k2, Hashable k2) => (k1 -> k2) -> HashMap k1 v -> HashMap k2 v
mapKeys convert = M.toList >>> fmap (first convert) >>> M.fromList
-- data NVA s sta i o c = NVA
--   { initial :: HashSet s
--   , innerTransitions :: HashMap s (HashMap i (HashSet s))
--   , openTransitions :: HashMap s (HashMap o (HashSet (sta, s)))
--   , closeTransitions :: HashMap s (HashMap c (HashSet (sta, s)))
--   , final :: HashSet s }

asNVA :: ( Eq s, Hashable s
         , Eq sta, Hashable sta)
      => DVA s sta i o c -> NVA s sta i o c
asNVA DVA{..} = NVA
  { NVA.initial = S.singleton initial
  , NVA.innerTransitions = innerTransitions <&> fmap S.singleton
  , NVA.openTransitions = openTransitions <&> fmap S.singleton
  , NVA.closeTransitions = closeTransitions <&> fmap (M.toList >>> S.fromList)
  , NVA.final = final }
