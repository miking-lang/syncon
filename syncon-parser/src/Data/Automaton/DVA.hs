{-# LANGUAGE RecordWildCards #-}

module Data.Automaton.DVA where

import Pre

import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S

import Util (iterateInductivelyOptM, repeatUntilStable)

import Data.Automaton.EpsilonNVA (TaggedTerminal(..))
import Data.Automaton.NVA (NVA(NVA))
import qualified Data.Automaton.NVA as NVA

data DVA s sta i o c = DVA
  { initial :: s
  , innerTransitions :: HashMap s (HashMap i s)
  , openTransitions :: HashMap s (HashMap o (sta, s))
  , closeTransitions :: HashMap s (HashMap c (HashMap sta s))
  , final :: HashSet s }

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
      { final = S.filter (fold >>> S.intersection fin >>> S.null >>> not) (states dva) }

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

states :: (Eq s, Hashable s) => DVA s sta i o c -> HashSet s
states DVA{..} = final <> S.fromList (is <> os <> cs)
  where
    is = M.keys innerTransitions <> (toList innerTransitions >>= toList)
    os = M.keys openTransitions <> (toList openTransitions >>= fmap snd . toList)
    cs = M.keys closeTransitions <> (toList closeTransitions >>= toList >>= toList)

dvaOp :: (Bool -> Bool -> Bool) -> DVA s1 sta1 i o c -> DVA s2 sta2 i o c -> DVA (s1, s2) (sta1, sta2) i o c
dvaOp = undefined

product :: DVA s1 sta1 i o c -> DVA s2 sta2 i o c -> DVA (s1, s2) (sta1, sta2) i o c
product = dvaOp (&&)

difference :: DVA s1 sta1 i o c -> DVA s2 sta2 i o c -> DVA (s1, s2) (sta1, sta2) i o c
difference = dvaOp (\a b -> a && not b)

renumberStates :: DVA s sta i o c -> DVA Int sta i o c
renumberStates = undefined

renumberStack :: DVA s sta i o c -> DVA s Int i o c
renumberStack = undefined

renumber :: DVA s sta i o c -> DVA Int Int i o c
renumber = renumberStates >>> renumberStack

shortestWord :: DVA s sta i o c -> Maybe [TaggedTerminal i o c]
shortestWord = undefined
