{-# LANGUAGE RecordWildCards #-}

module ParenAutomaton
( ParenNFA(..)
, fromLanguage
, RegexAlphabet(..)
, Language
, addDyck
, product
, asNFA
, FakeEdge(..)
, ppFakeEdge
, mapSta
, size
) where

import Pre hiding (product)

import Data.String (fromString)

import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S

import Util (iterateInductivelyOptM)

import qualified Automaton as FA
import qualified Automaton.NFA as N
import qualified Automaton.DFA as D
import qualified Automaton.EpsilonNFA as E
import qualified Regex as R

data ParenNFA s sta a = ParenNFA
  { initial :: s
  , innerTransitions :: HashMap s (HashMap a (HashSet s))
  , openTransitions :: HashMap s (HashSet (sta, s))
  , closeTransitions :: HashMap s (HashSet (sta, s))
  , final :: s }

data RegexAlphabet nt t = NT nt | T t deriving (Eq, Generic)
instance IsString t => IsString (RegexAlphabet nt t) where
  fromString = fromString >>> T
instance (Hashable nt, Hashable t) => Hashable (RegexAlphabet nt t)
type Language nt t = HashMap nt [(R.Regex (RegexAlphabet nt t))]

fromLanguage :: forall nt t. (Eq nt, Hashable nt, Eq t, Hashable t)
             => nt -> Language nt t -> ParenNFA Int (Int, Int) t
fromLanguage startNT language = renumber complete
  where
    nfas = M.mapWithKey toMinimalDFA language
    toMinimalDFA k = fmap (R.toAutomaton >>> E.determinize >>> D.minimize)
      >>> fmap (D.renumber >>> fst)
      >>> zip [(1::Int)..]
      >>> fmap (\(i, dfa) -> FA.mapState (k, i, ) dfa & D.asNFA)

    initials = foldMap (N.initial >>> S.singleton) <$> nfas
    finals = foldMap N.final <$> nfas

    allTransitions = foldMap (fmap N.transitions) nfas
      & foldl N.mergeTransitions M.empty
      & toTriples
    classifyTransition (s1, NT nt, s2) = Left (s1, nt, s2)
    classifyTransition (s1, T t, s2) = Right (s1, t, s2)
    (ntTransitions, tTransitions) = classifyTransition <$> allTransitions
      & partitionEithers

    mkOpen (s1, nt, s2) = M.lookupDefault S.empty nt initials
      & S.map ((s1, s2),)
      & M.singleton s1
    mkClose (s1, nt, s2) = M.lookupDefault S.empty nt finals
      & S.toMap
      & (S.singleton ((s1, s2), s2) <$)

    initialState = (startNT, -1, 0)
    finalState = (startNT, -1, 1)
    topTransition = (initialState, startNT, finalState)
    complete = ParenNFA
      { innerTransitions = fromTriples tTransitions
      , openTransitions = mkOpen <$> (topTransition : ntTransitions)
        & foldl (M.unionWith S.union) M.empty
      , closeTransitions = mkClose <$> (topTransition : ntTransitions)
        & foldl (M.unionWith S.union) M.empty
      , initial = initialState
      , final = finalState }

    renumber :: (Eq s, Hashable s) => ParenNFA s (s, s) a -> ParenNFA Int (Int, Int) a
    renumber nfa@ParenNFA{..} = ParenNFA
      { initial = convert initial
      , final = convert final
      , innerTransitions = M.toList innerTransitions
        & fmap (convert *** fmap (S.map convert))
        & M.fromList
      , openTransitions = M.toList openTransitions
        & fmap (convert *** S.map ((convert *** convert) *** convert))
        & M.fromList
      , closeTransitions = M.toList closeTransitions
        & fmap (convert *** S.map ((convert *** convert) *** convert))
        & M.fromList }
      where
        convert s = M.lookup s translationMap
          & compFromJust "ParenAutomaton.fromLanguage.renumber.convert" "missing state"
        translationMap = M.fromList $ S.toList (states nfa) `zip` [1..]

    fromTriples :: forall a b. (Eq a, Hashable a, Eq b, Hashable b)
                => [(a, b, a)] -> HashMap a (HashMap b (HashSet a))
    fromTriples = fmap (\(a, b, c) -> M.singleton a $ M.singleton b $ S.singleton c)
      >>> foldl N.mergeTransitions M.empty

states :: (Eq s, Hashable s) => ParenNFA s sta a -> HashSet s
states ParenNFA{..} = S.fromList (initial : final : inners ++ opens ++ closes)
  where
    inners = toTriples innerTransitions
      & concatMap (\(s1, _, s2) -> [s1, s2])
    opens = M.keys openTransitions
      ++ (S.toList `concatMap` M.elems openTransitions & fmap snd)
    closes = M.keys closeTransitions
      ++ (S.toList `concatMap` M.elems closeTransitions & fmap snd)

toTriples :: forall a b c. HashMap a (HashMap b (HashSet c)) -> [(a, b, c)]
toTriples trs = do
  (a, bs) <- M.toList trs
  (b, cs) <- M.toList bs
  c <- S.toList cs
  return $ (a, b, c)

addDyck :: (Eq s, Hashable s, Eq sta, Hashable sta) => ParenNFA s sta a -> ParenNFA s (Maybe sta) a
addDyck nfa@ParenNFA{initial, openTransitions, closeTransitions, final} = nfa
  { openTransitions = S.map (first Just) <$> openTransitions
    & M.unionWith S.union addedTransitions
  , closeTransitions = S.map (first Just) <$> closeTransitions
    & M.unionWith S.union addedTransitions}
  where
    addedTransitions = states nfa
      & S.delete initial
      & S.delete final
      & S.toMap
      & M.mapWithKey (\k _ -> S.singleton (Nothing, k))

mapSta :: (Eq s, Hashable s, Eq stb, Hashable stb)
       => (sta -> stb) -> ParenNFA s sta a -> ParenNFA s stb a
mapSta f nfa@ParenNFA{openTransitions, closeTransitions} = nfa
  { openTransitions = S.map (first f) <$> openTransitions
  , closeTransitions = S.map (first f) <$> closeTransitions }

data ProductState s sta a = ProductState
  { inners :: HashMap s (HashMap a (HashSet s))
  , opens :: HashMap s (HashSet (sta, s))
  , closes :: HashMap s (HashSet (sta, s)) }

instance (Eq s, Hashable s, Eq sta, Hashable sta, Eq a, Hashable a) => Semigroup (ProductState s sta a) where
  ProductState i1 o1 c1 <> ProductState i2 o2 c2 = ProductState
    (M.unionWith (M.unionWith S.union) i1 i2)
    (M.unionWith S.union o1 o2)
    (M.unionWith S.union c1 c2)
instance (Eq s, Hashable s, Eq sta, Hashable sta, Eq a, Hashable a) => Monoid (ProductState s sta a) where
  mempty = ProductState mempty mempty mempty
  mappend = (<>)

product :: (Eq s, Hashable s, Eq sta, Hashable sta, Eq a, Hashable a)
        => ParenNFA s sta a -> ParenNFA s sta a -> ParenNFA (s, s) (sta, sta) a
product nfa1 nfa2 = ParenNFA
  { initial = (i1, i2)
  , final = (f1, f2)
  , innerTransitions = inners foundTransitions
  , openTransitions = opens foundTransitions
  , closeTransitions = closes foundTransitions }
  where
    ParenNFA{initial = i1, final = f1, innerTransitions = it1, openTransitions = ot1, closeTransitions = ct1} = nfa1
    ParenNFA{initial = i2, final = f2, innerTransitions = it2, openTransitions = ot2, closeTransitions = ct2} = nfa2

    findTransitions s@(s1, s2) = do
      let is = M.intersectionWith cartesianProduct
                 (M.lookupDefault M.empty s1 it1)
                 (M.lookupDefault M.empty s2 it2)
          os = cartesianProduct'
                 (M.lookupDefault S.empty s1 ot1)
                 (M.lookupDefault S.empty s2 ot2)
          cs = cartesianProduct'
                 (M.lookupDefault S.empty s1 ct1)
                 (M.lookupDefault S.empty s2 ct2)
          newStates = fold is `S.union` S.map snd os `S.union` S.map snd cs

      addTransitions $ ProductState
        { inners = M.singleton s is
        , opens = M.singleton s os
        , closes = M.singleton s cs }

      return newStates

    foundTransitions = execState
      (iterateInductivelyOptM findTransitions $ S.singleton (i1, i2))
      (ProductState M.empty M.empty M.empty)

    cartesianProduct :: (Eq a, Hashable a, Eq b, Hashable b)
                     => HashSet a -> HashSet b -> HashSet (a, b)
    cartesianProduct as bs = (,) <$> S.toList as <*> S.toList bs & S.fromList

    cartesianProduct' :: ( Eq a, Hashable a, Eq sta, Hashable sta
                         , Eq b, Hashable b, Eq stb, Hashable stb )
                      => HashSet (sta, a) -> HashSet (stb, b) -> HashSet ((sta, stb), (a, b))
    cartesianProduct' as bs = (\(sta, a) (stb, b) -> ((sta, stb), (a, b)))
      <$> S.toList as <*> S.toList bs & S.fromList

    addTransitions transitions = modify (mappend transitions)

asNFA :: (Eq s, Hashable s, Eq sta, Hashable sta, Eq a, Hashable a) => ParenNFA s sta a -> N.NFA s (FakeEdge sta a)
asNFA ParenNFA{..} = N.NFA
  { N.initial = initial
  , N.final = S.singleton final
  , N.transitions = mapKey Inner <$> innerTransitions
    & N.mergeTransitions (toMap Push <$> openTransitions)
    & N.mergeTransitions (toMap Pop <$> closeTransitions) }
  where
    mapKey f = M.toList >>> fmap (first f) >>> M.fromList
    toMap f = S.toList >>> fmap (f *** S.singleton) >>> M.fromListWith S.union

data FakeEdge sta a = Push sta | Pop sta | Inner a deriving (Eq, Show, Generic)
instance (Hashable sta, Hashable a) => Hashable (FakeEdge sta a)

ppFakeEdge :: (Show sta, Show a) => FakeEdge sta a -> Text
ppFakeEdge (Inner a) = show a
ppFakeEdge e = show e

data Size = Size { numStates :: Int, numInner :: Int, numOpen :: Int, numClose :: Int } deriving Show

size :: (Eq s, Hashable s) => ParenNFA s sta a -> Size
size nfa@ParenNFA{..} = Size
  { numStates = S.size $ states nfa
  , numInner = foldMap (foldMap $ S.size >>> Sum) innerTransitions & getSum
  , numOpen = foldMap (S.size >>> Sum) openTransitions & getSum
  , numClose = foldMap (S.size >>> Sum) closeTransitions & getSum }
