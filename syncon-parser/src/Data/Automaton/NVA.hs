{-# LANGUAGE RecordWildCards #-}

module Data.Automaton.NVA where

import Pre hiding (product, reverse, reduce, all, from, to, sym, check)

import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S

import Util (iterateInductivelyOptM, iterateInductively)

-- | A visibly pushdown automaton with
-- * 's' as the states
-- * 'sta' as the stack alphabet
-- * 'i' as the alphabet for inner transitions
-- * 'o' as the alphabet for call transitions (mnemonic "open")
-- * 'c' as the alphabet for return transitions (mnemonic "close")
data NVA s sta i o c = NVA
  { initial :: HashSet s
  , innerTransitions :: HashMap s (HashMap i (HashSet s))
  , openTransitions :: HashMap s (HashMap o (HashSet (sta, s)))
  , closeTransitions :: HashMap s (HashMap c (HashSet (sta, s)))
  , final :: HashSet s }

-- | Retrieve all the states mentioned in a NVA
states :: (Eq s, Hashable s) => NVA s sta i o c -> HashSet s
states NVA{..} = initial `S.union` final `S.union` S.fromList (inners ++ opens ++ closes)
  where
    inners = toTriples innerTransitions
      & concatMap (\(s1, _, s2) -> [s1, s2])
    opens = toTriples openTransitions
      & concatMap (\(s1, _, (_, s2)) -> [s1, s2])
    closes = toTriples closeTransitions
      & concatMap (\(s1, _, (_, s2)) -> [s1, s2])

-- | Convert a transition map to a list of triples, for easier relational reasoning
toTriples :: forall a b c. HashMap a (HashMap b (HashSet c)) -> [(a, b, c)]
toTriples trs = do
  (a, bs) <- M.toList trs
  (b, cs) <- M.toList bs
  c <- S.toList cs
  return $ (a, b, c)

-- | The inverse of 'toTriples'
fromTriples :: forall a b c. (Eq a, Hashable a, Eq b, Hashable b, Eq c, Hashable c)
            => [(a, b, c)] -> HashMap a (HashMap b (HashSet c))
fromTriples = fmap (\(a, b, c) -> M.singleton a $ M.singleton b $ S.singleton c)
  >>> foldl (M.unionWith $ M.unionWith S.union) M.empty

-- | Reverse an NVA, i.e., create a new NVA that recognizes the same language, except every word is reversed
reverse :: forall s sta i o c.
           ( Eq s, Hashable s
           , Eq i, Hashable i
           , Eq c, Hashable c
           , Eq sta, Hashable sta
           , Eq o, Hashable o )
        => NVA s sta i o c -> NVA s sta i c o
reverse NVA{..} = NVA
  { initial = final
  , final = initial
  , innerTransitions = toTriples innerTransitions
    & fmap (\(a, b, c) -> (c, b, a))
    & fromTriples
  , openTransitions = toTriples closeTransitions
    & fmap (\(a, b, (sta, c)) -> (c, b, (sta, a)))
    & fromTriples
  , closeTransitions = toTriples openTransitions
    & fmap (\(a, b, (sta, c)) -> (c, b, (sta, a)))
    & fromTriples }

-- | Map over the stack symbols
mapSta :: forall s sta stb i o c. (Eq s, Hashable s , Eq stb, Hashable stb)
       => (sta -> stb) -> NVA s sta i o c -> NVA s stb i o c
mapSta f nva@NVA{openTransitions, closeTransitions} = nva
  { openTransitions = fmap (S.map $ first f) <$> openTransitions
  , closeTransitions = fmap (S.map $ first f) <$> closeTransitions }

-- |
-- = Product Automaton

-- | State to keep track of the currently discovered transitions
data ProductState s sta i o c = ProductState
  { inners :: HashMap s (HashMap i (HashSet s))
  , opens :: HashMap s (HashMap o (HashSet (sta, s)))
  , closes :: HashMap s (HashMap c (HashSet (sta, s))) }

instance
  ( Eq s, Hashable s
  , Eq sta, Hashable sta
  , Eq i, Hashable i
  , Eq o, Hashable o
  , Eq c, Hashable c)
  => Semigroup (ProductState s sta i o c) where
  ProductState i1 o1 c1 <> ProductState i2 o2 c2 = ProductState
    (M.unionWith (M.unionWith S.union) i1 i2)
    (M.unionWith (M.unionWith S.union) o1 o2)
    (M.unionWith (M.unionWith S.union) c1 c2)
instance
  ( Eq s, Hashable s
  , Eq sta, Hashable sta
  , Eq i, Hashable i
  , Eq o, Hashable o
  , Eq c, Hashable c)
  => Monoid (ProductState s sta i o c) where
  mempty = ProductState mempty mempty mempty
  mappend = (<>)

-- | Constructing a product automaton from two NVAs with the same alphabet partition
product :: forall s1 sta1 s2 sta2 i o c.
           ( Eq s1, Hashable s1
           , Eq sta1, Hashable sta1
           , Eq sta2, Hashable sta2
           , Eq s2, Hashable s2
           , Eq i, Hashable i
           , Eq o, Hashable o
           , Eq c, Hashable c )
        => NVA s1 sta1 i o c -> NVA s2 sta2 i o c -> NVA (s1, s2) (sta1, sta2) i o c
product nva1 nva2 = NVA
  { initial = newInitial
  , final = S.empty
  , innerTransitions = inners foundTransitions
  , openTransitions = opens foundTransitions
  , closeTransitions = closes foundTransitions }
  & adjustFinal
  where
    NVA{initial = i1, final = f1, innerTransitions = it1, openTransitions = ot1, closeTransitions = ct1} = nva1
    NVA{initial = i2, final = f2, innerTransitions = it2, openTransitions = ot2, closeTransitions = ct2} = nva2
    newInitial = cartesianProduct i1 i2

    adjustFinal nva = nva
      { final = S.filter (\(s1, s2) -> S.member s1 f1 && S.member s2 f2) (states nva) }

    -- Given a state in the product automaton, find all transitions from it and the resulting states
    findTransitions s@(s1, s2) = do
      let is = M.intersectionWith cartesianProduct
               (M.lookupDefault M.empty s1 it1)
               (M.lookupDefault M.empty s2 it2)
          os = M.intersectionWith cartesianProduct'
               (M.lookupDefault M.empty s1 ot1)
               (M.lookupDefault M.empty s2 ot2)
          cs = M.intersectionWith cartesianProduct'
               (M.lookupDefault M.empty s1 ct1)
               (M.lookupDefault M.empty s2 ct2)
          newStates = fold is `S.union` foldMap (S.map snd) os `S.union` foldMap (S.map snd) cs

      addTransitions $ ProductState
        { inners = M.singleton s is
        , opens = M.singleton s os
        , closes = M.singleton s cs }

      return newStates

    -- The complete set of transitions in the product automaton
    foundTransitions = execState
      (iterateInductivelyOptM findTransitions $ newInitial)
      mempty

    -- Small helpers
    addTransitions transitions = modify (mappend transitions)

    cartesianProduct :: (Eq a, Hashable a, Eq b, Hashable b)
                     => HashSet a -> HashSet b -> HashSet (a, b)
    cartesianProduct as bs = (,) <$> S.toList as <*> S.toList bs & S.fromList

    cartesianProduct' :: ( Eq a, Hashable a, Eq sta, Hashable sta
                         , Eq b, Hashable b, Eq stb, Hashable stb )
                      => HashSet (sta, a) -> HashSet (stb, b) -> HashSet ((sta, stb), (a, b))
    cartesianProduct' as bs = (\(sta, a) (stb, b) -> ((sta, stb), (a, b)))
      <$> S.toList as <*> S.toList bs & S.fromList

-- |
-- = Reducing an NVA

-- | Removes a few obviously not co-reachable states, namely those that don't have a path to the
--   final state even when we ignore the stack.
trivialCoReduce :: forall s sta i o c.
                   ( Eq s, Hashable s
                   , Eq sta, Hashable sta
                   , Eq i, Hashable i
                   , Eq o, Hashable o
                   , Eq c, Hashable c )
                => NVA s sta i o c -> NVA s sta i o c
trivialCoReduce nva@NVA{..} = nva
  { innerTransitions = toTriples innerTransitions
    & filter isCoReachable
    & fromTriples
  , openTransitions = toTriples openTransitions
    & filter isCoReachable'
    & fromTriples
  , closeTransitions = toTriples closeTransitions
    & filter isCoReachable'
    & fromTriples }
  where
    revInner = toTriples innerTransitions
      & fmap (\(s1, _, s2) -> (s2, S.singleton s1))
    revOpen = toTriples openTransitions
      & fmap (\(s1, _, (_, s2)) -> (s2, S.singleton s1))
    revClose = toTriples closeTransitions
      & fmap (\(s1, _, (_, s2)) -> (s2, S.singleton s1))

    revMap :: HashMap s (HashSet s)
    revMap = M.fromListWith S.union $ revInner ++ revOpen ++ revClose

    coReachable :: HashSet s
    coReachable = iterateInductively (\s -> M.lookupDefault S.empty s revMap) final

    isCoReachable :: forall x. (s, x, s) -> Bool
    isCoReachable (s1, _, s2) = S.member s1 coReachable && S.member s2 coReachable
    isCoReachable' :: forall x y. (s, x, (y, s)) -> Bool
    isCoReachable' (s1, _, (_, s2)) = S.member s1 coReachable && S.member s2 coReachable
