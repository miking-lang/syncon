{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Automaton.NVA where

import Pre hiding (product, reverse, reduce, all, from, to, sym, check, concat)
import qualified Pre

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.Vector (Vector)
import qualified Data.Vector as Vec

import qualified Data.Automaton.NFA as N
import Data.Automaton.EpsilonNVA (EpsNVA(EpsNVA), TaggedTerminal(..))
import qualified Data.Automaton.EpsilonNVA as EpsNVA

import Util (iterateInductivelyOptM, iterateInductively)

-- | A visibly pushdown automaton with
-- * 's' as the states
-- * 'sta' as the stack alphabet
-- * 'i' as the alphabet for inner transitions
-- * 'o' as the alphabet for call transitions (mnemonic "open")
-- * 'c' as the alphabet for return transitions (mnemonic "close")
data NVA s sta i o c = NVA
  { initial :: !(HashSet s)
  , innerTransitions :: !(HashMap s (HashMap i (HashSet s)))
  , openTransitions :: !(HashMap s (HashMap o (HashSet (sta, s))))
  , closeTransitions :: !(HashMap s (HashMap c (HashSet (sta, s))))
  , final :: !(HashSet s) }
  deriving (Show, Eq, Generic)
instance (Hashable s, Hashable sta, Hashable i, Hashable o, Hashable c) => Hashable (NVA s sta i o c)

-- | Retrieve all the states mentioned in an NVA
states :: (Eq s, Hashable s) => NVA s sta i o c -> HashSet s
states NVA{..} = initial `S.union` final `S.union` S.fromList (inners ++ opens ++ closes)
  where
    inners = toTriples innerTransitions
      & concatMap (\(s1, _, s2) -> [s1, s2])
    opens = toTriples openTransitions
      & concatMap (\(s1, _, (_, s2)) -> [s1, s2])
    closes = toTriples closeTransitions
      & concatMap (\(s1, _, (_, s2)) -> [s1, s2])

stackSymbols :: (Eq sta, Hashable sta) => NVA s sta i o c -> HashSet sta
stackSymbols NVA{openTransitions,closeTransitions} =
  (foldMap (foldMap $ S.map fst) openTransitions)
  <> (foldMap (foldMap $ S.map fst) closeTransitions)

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

-- | Map over the states
mapStates :: (Eq s2, Hashable s2, Eq sta, Hashable sta)
          => (s1 -> s2) -> NVA s1 sta i o c -> NVA s2 sta i o c
mapStates convert NVA{..} = NVA
  { initial = S.map convert initial
  , innerTransitions = mapKeys convert innerTransitions
    <&> fmap (S.map convert)
  , openTransitions = mapKeys convert openTransitions
    <&> fmap (S.map $ second convert)
  , closeTransitions = mapKeys convert closeTransitions
    <&> fmap (S.map $ second convert)
  , final = S.map convert final }

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

data ReduceState s = ReduceState
  { _all :: HashSet (s, s)
  , _lToR :: HashMap s (HashSet s)
  , _rToL :: HashMap s (HashSet s) }

instance (Eq s, Hashable s) => Semigroup (ReduceState s) where
  ReduceState a1 l1 r1 <> ReduceState a2 l2 r2 = ReduceState
    (a1 <> a2)
    (M.unionWith S.union l1 l2)
    (M.unionWith S.union r1 r2)
instance (Eq s, Hashable s) => Monoid (ReduceState s) where
  mempty = ReduceState mempty mempty mempty
  mappend = (<>)

type ReduceM s a = Reader (ReduceState s) a

-- TODO: I suspect this function could be written much nicer with something relational
reduce :: forall s sta i o c.
          ( Eq s, Hashable s
          , Eq sta, Hashable sta
          , Eq i, Hashable i
          , Eq o, Hashable o
          , Eq c, Hashable c )
       => NVA s sta i o c -> NVA (s, s) (sta, s) i o c
reduce nfa@NVA{..} = NVA
  { initial = initialStates
  , openTransitions = opens foundTransitions
  , closeTransitions = closes foundTransitions
  , innerTransitions = inners foundTransitions
  , final = S.empty }
  & adjustFinal
  where
    adjustFinal nva = nva
      { final = states nva & S.filter (\(a, b) -> a == b && S.member a final) }
    initialStates = S.filter (\(a, b) -> S.member a initial && S.member b final) wellMatched

    innerMap = fold <$> innerTransitions
    innerAfter s = M.lookupDefault S.empty s innerMap & S.toList

    openMap = toTriples openTransitions
      & fmap (\(s1, _, (sta, s2)) -> (s2, M.singleton sta $ S.singleton s1))
      & M.fromListWith (M.unionWith S.union)
    openBefore s = M.lookupDefault M.empty s openMap

    closeMap = toTriples closeTransitions
      & fmap (\(s1, _, (sta, s2)) -> (s1, M.singleton sta $ S.singleton s2))
      & M.fromListWith (M.unionWith S.union)
    closeAfter s = M.lookupDefault M.empty s closeMap

    findNewWellMatched :: (s, s) -> ReduceM s [(s, s)]
    findNewWellMatched (s1, s2) = do
      after <- fmap (s1,) <$> lToR s2
      before <- fmap (,s2) <$> rToL s1
      let inner = (s1,) <$> innerAfter s2
          wm = M.intersectionWith
                 (\a' b' -> (,) <$> toList a' <*> toList b')
                 (openBefore s1)
                 (closeAfter s2)
            & fold
      return $ after <> before <> inner <> wm

    wellMatched = states nfa
      & S.map (identity &&& identity)
      & iterateInductivelyR findNewWellMatched

    pcgammap' :: HashMap s (HashMap o (HashMap sta (HashSet s)))
    pcgammap' = openTransitions
      <&> fmap (toList >>> fmap (second S.singleton) >>> M.fromListWith S.union)

    sgammaq' :: HashMap s (HashMap sta (HashSet s))
    sgammaq' = toTriples closeTransitions
      <&> (\(q', _, (gamma, s)) -> (s, gamma, q'))
      & fromTriples

    qgammaq' :: HashMap s (HashMap sta (HashSet s))
    qgammaq' = toList wellMatched
      <&> (snd &&& (fst >>> S.singleton))
      & M.fromListWith S.union  -- backwards map for wellMatched
      & fmap (S.toMap
              >>> M.intersection sgammaq'
              >>> foldl' (M.unionWith S.union) M.empty)

    q'rgammapq :: HashMap s (HashMap c (HashSet ((sta, s), (s, s))))
    q'rgammapq = closeTransitions
      <&> fmap (toList >>> fmap (second onlyP) >=> mkIntermediate)
      <&> fmap S.fromList
      where
        pTopq = toList wellMatched <&> (fst &&& S.singleton) & M.fromListWith S.union
        onlyP p = M.lookupDefault S.empty p pTopq
        mkIntermediate (gamma, pq) = toList pq
          <&> \(p, q) -> ((gamma, q), (p, q))

    -- This (plus the intermediate results above) kinda screams relational algebra
    findTransitions (p, q) = do
      let os = M.lookupDefault M.empty p pcgammap'
            <&> M.intersectionWith (flip cartesianProduct) (M.lookupDefault M.empty q qgammaq') -- have a cgamma(p',q')
            <&> fmap (S.intersection wellMatched)  -- have a cgamma(p',q') where (p', q') are in wellMatched
            <&> (M.toList >=> (\(sta, pqs) -> ((sta, q),) <$> toList pqs))
            <&> S.fromList
          is = M.lookupDefault M.empty p innerTransitions
            <&> S.map (,q)
            <&> S.intersection wellMatched
          cs | p /= q = M.empty
             | otherwise = M.lookupDefault M.empty p q'rgammapq
          newStates = fold is <> foldMap (S.map snd) os <> foldMap (S.map snd) cs
      addTransitions $ ProductState
        { inners = M.singleton (p, q) is
        , opens = M.singleton (p, q) os
        , closes = M.singleton (p, q) cs }

      return newStates

    foundTransitions = execState
      (iterateInductivelyOptM findTransitions initialStates)
      mempty

    addTransitions transitions = modify $ mappend transitions

    iterateInductivelyR :: forall x. (Eq x, Hashable x)
                        => ((x, x) -> ReduceM x [(x, x)]) -> HashSet (x, x) -> HashSet (x, x)
    iterateInductivelyR f init = recur mempty init
      where
        recur :: ReduceState x -> HashSet (x, x) -> HashSet (x, x)
        recur prev new
          | S.null new = _all prev
          | otherwise =
            let all = prev <> mkReduceState new
                next = S.toList new & traverse f & (`runReader` all) & join & S.fromList
                newNext = next `S.difference` _all all
            in recur all newNext

    mkReduceState ss = ReduceState
      { _all = ss
      , _lToR = S.toList ss & fmap (second S.singleton) & M.fromListWith S.union
      , _rToL = S.toList ss & fmap (swap >>> second S.singleton) & M.fromListWith S.union }

    lToR :: s -> ReduceM s [s]
    lToR s = asks (_lToR >>> M.lookupDefault S.empty s >>> S.toList)
    rToL :: s -> ReduceM s [s]
    rToL s = asks (_rToL >>> M.lookupDefault S.empty s >>> S.toList)

    cartesianProduct :: (Eq a, Hashable a, Eq b, Hashable b)
                     => HashSet a -> HashSet b -> HashSet (a, b)
    cartesianProduct as bs = (,) <$> S.toList as <*> S.toList bs & S.fromList

fromEpsNVA :: forall s sta i o c.
              ( Eq s, Hashable s
              , Eq sta, Hashable sta
              , Eq i, Hashable i
              , Eq o, Hashable o
              , Eq c, Hashable c )
           => EpsNVA s sta i o c -> NVA (HashSet s) sta i o c
fromEpsNVA EpsNVA{..} = NVA
  { initial = newInitial
  , innerTransitions = inners foundTransitions
  , openTransitions = opens foundTransitions
  , closeTransitions = closes foundTransitions
  , final = S.empty }
  & adjustFinal
  where
    adjustFinal nva = nva
     { final = states nva & S.filter (any (`S.member` final)) }

    epsTrans s = M.lookup s innerTransitions >>= M.lookup Nothing & fold
    epsClose = iterateInductively epsTrans

    newInitial :: HashSet (HashSet s)
    newInitial = S.map (S.singleton >>> epsClose) initial

    foundTransitions = execState
      (iterateInductivelyOptM findTransitions $ newInitial)
      mempty

    findTransitions s = do
      let is = innerTransitions `M.intersection` S.toMap s
            & fmap (fmap $ S.map $ S.singleton >>> epsClose)
            & foldl' (M.unionWith S.union) M.empty
            & M.toList
            & mapMaybe (\(ma, b) -> (, b) <$> ma)
            & M.fromList
          os = openTransitions `M.intersection` S.toMap s
            & fmap (fmap $ S.map $ second $ S.singleton >>> epsClose)
            & foldl' (M.unionWith S.union) M.empty
          cs = closeTransitions `M.intersection` S.toMap s
            & fmap (fmap $ S.map $ second $ S.singleton >>> epsClose)
            & foldl' (M.unionWith S.union) M.empty
          newStates = fold is <> foldMap (S.map snd) os <> foldMap (S.map snd) cs

      addTransitions $ ProductState
        { inners = M.singleton s is
        , opens = M.singleton s os
        , closes = M.singleton s cs }

      return newStates

    addTransitions transitions = modify (mappend transitions)

renumberStates :: forall s sta i o c.
                  ( Eq s, Hashable s
                  , Eq sta, Hashable sta )
               => NVA s sta i o c -> NVA Int sta i o c
renumberStates nva = mapStates oldToNew nva
  where
    oldToNew s = M.lookup s newStates
      & compFromJust "Data.Automaton.NVA.renumberStates" "missing state"
    newStates :: HashMap s Int
    newStates = evalState computeNewStates 0
    computeNewStates = states nva & S.toMap & traverse (\_ -> get <* modify (+1))

renumberStack :: ( Eq s, Hashable s
                 , Eq sta, Hashable sta )
              => NVA s sta i o c -> NVA s Int i o c
renumberStack nva = mapSta oldToNew nva
  where
    oldToNew sta = M.lookup sta newSymbols
      & compFromJust "Data.Automaton.NVA.renumberStack" "missing symbol"
    newSymbols = evalState computeNewSymbols 0
    computeNewSymbols = stackSymbols nva & S.toMap & traverse (\_ -> get <* modify (+1))

renumber :: ( Eq s, Hashable s
            , Eq sta, Hashable sta )
         => NVA s sta i o c -> NVA Int Int i o c
renumber = renumberStates >>> renumberStack

trim :: ( Eq s, Hashable s
        , Eq sta, Hashable sta
        , Eq i, Hashable i
        , Eq o, Hashable o
        , Eq c, Hashable c )
     => NVA s sta i o c -> NVA Int Int i o c
trim = reverse >>> reduce >>> reverse >>> reduce >>> trivialCoReduce >>> renumber

-- | Produce the shortest word in a non-empty or trim NVA, if there is one.
-- WARNING: if the NVA represents an empty language, but isn't trim, this might not terminate
shortestWord :: forall s sta i o c.
                ( Eq s, Hashable s
                , Eq sta, Hashable sta
                , Eq i, Hashable i
                , Eq o, Hashable o
                , Eq c, Hashable c )
             => NVA s sta i o c -> Maybe [TaggedTerminal i o c]
shortestWord NVA{..} = recur initialConfigs
  where
    initialConfigs :: HashSet (s, [sta], [TaggedTerminal i o c])
    initialConfigs = S.map (,[],[]) initial

    recur :: HashSet (s, [sta], [TaggedTerminal i o c]) -> Maybe [TaggedTerminal i o c]
    recur configs
      | S.null configs = Nothing
      | Just (_, _, word) <- find isFinal configs = Just $ Pre.reverse word
      | otherwise = foldMap step configs & recur

    closeBySta :: HashMap s (HashMap sta (HashSet (c, s)))
    closeBySta = closeTransitions
      <&> (M.toList >=> \(c, stas) -> toList stas <&> (c,))
      <&> fmap (\(c, (sta, s)) -> (sta, S.singleton (c, s)))
      <&> M.fromListWith S.union

    step :: (s, [sta], [TaggedTerminal i o c]) -> HashSet (s, [sta], [TaggedTerminal i o c])
    step (s, stack, word) =
      let is = M.lookupDefault M.empty s innerTransitions
            & M.toList
            & foldMap (\(i, ss) -> S.map (\s2 -> (s2, stack, Inner i : word)) ss)
          os = M.lookupDefault M.empty s openTransitions
            & M.toList
            & foldMap (\(o, stas) -> S.map (\(sta, s2) -> (s2, sta : stack, Open o : word)) stas)
          cs | sta : stack' <- stack = M.lookup s closeBySta >>= M.lookup sta
               & foldMap (S.map $ \(c, s2) -> (s2, stack', Close c : word))
             | otherwise = S.empty
      in is <> os <> cs

    isFinal :: forall x. (s, [sta], x) -> Bool
    isFinal (s, [], _) = S.member s final
    isFinal _ = False

-- TODO: make a timeouting version of this in IO
shortestUniqueWord :: forall s sta i o c.
                      ( Eq s, Hashable s
                      , Eq sta, Hashable sta
                      , Eq i, Hashable i
                      , Eq o, Hashable o
                      , Eq c, Hashable c )
                   => Int  -- ^ Max word length to consider (inclusive)
                   -> HashSet (NVA s sta i o c)  -- ^ NVAs to find unique words for
                   -> HashMap (NVA s sta i o c) [TaggedTerminal i o c]  -- ^ Produced shortest unique words, per NVA. If no unique word was found for an NVA, then there is no entry for that NVA here
shortestUniqueWord maxLength _
  | maxLength < 0 = compErr "Data.Automaton.NVA.shortestUniqueWord" $ "Got a negative maxLength: " <> show maxLength
shortestUniqueWord maxLength (toList >>> Vec.fromList -> nvas) =
  recur maxLength (getUniques initials) initials
  <&> Pre.reverse
  where
    asNVA idx = Vec.unsafeIndex nvas idx
    initialConfigs NVA{initial} = S.map (, []) initial
    initials = nvas
      <&> initialConfigs
      & M.singleton []

    recur :: Int  -- ^ max amount of further steps to take
          -> HashMap Int [TaggedTerminal i o c] -- ^ already found shortest words
          -> HashMap [TaggedTerminal i o c] (Vector (HashSet (s, [sta]))) -- ^ current configurations, grouped by word so far
          -> HashMap (NVA s sta i o c) [TaggedTerminal i o c]
    recur n result _
      | 0 <- n = result'
      | M.size result == Vec.length nvas = result'
      where
        result' = M.toList result <&> first asNVA & M.fromList
    recur n prev (clearRedundant prev -> configs) =
      recur (n-1) next steppedConfigs
      where
        steppedConfigs = configs
          <&> Vec.imap (\nva -> foldl' (\a b -> step nva b & M.unionWith S.union a) M.empty)
          <&> distribute
          & addTerminal
        next = M.union prev $ getUniques steppedConfigs

    getUniques :: HashMap [TaggedTerminal i o c] (Vector (HashSet (s, [sta]))) -> HashMap Int [TaggedTerminal i o c]
    getUniques configs = M.mapMaybe isUnique configs
      & M.toList
      <&> swap
      & M.fromList
    isUnique = Vec.indexed >>> toList >>> filter (\(nva, cs) -> any (isFinal $ asNVA nva) cs) >>> \case
      [(nva, _)] -> Just nva
      _ -> Nothing

    closeBySta :: Vector (HashMap s (HashMap sta (HashMap c (HashSet s))))
    closeBySta = nvas & fmap (\nva -> closeTransitions nva
      <&> (M.toList >=> \(c, stas) -> toList stas <&> (c,))
      <&> fmap (\(c, (sta, s)) -> (sta, M.singleton c $ S.singleton s))
      <&> M.fromListWith (M.unionWith S.union))

    step :: Int -> (s, [sta]) -> HashMap (TaggedTerminal i o c) (HashSet (s, [sta]))
    step nva@(asNVA -> NVA{innerTransitions,openTransitions}) (s, stack) =
      let is = M.lookupDefault M.empty s innerTransitions
            & M.toList
            <&> (Inner *** S.map (,stack))
            & M.fromListWith S.union
          os = M.lookupDefault M.empty s openTransitions
            & M.toList
            <&> (Open *** S.map (\(sta, s2) -> (s2, sta : stack)))
            & M.fromListWith S.union
          cs | sta : stack' <- stack = closeBySta Vec.!? nva >>= M.lookup s >>= M.lookup sta
               & foldMap (M.toList >>> fmap (Close *** S.map (,stack')) >>> M.fromList)
             | otherwise = M.empty
      in foldl' (M.unionWith S.union) M.empty [is, os, cs]

    -- | Remove the partial words that could only produce unique words for NVAs that already have a
    -- shortest word found.
    clearRedundant :: HashMap Int [TaggedTerminal i o c]
                   -> HashMap [TaggedTerminal i o c] (Vector (HashSet (s, [sta])))
                   -> HashMap [TaggedTerminal i o c] (Vector (HashSet (s, [sta])))
    clearRedundant alreadyFound = M.filter $
      Vec.indexed >>> any (\(nva, configs) -> not (S.null configs) && not (M.member nva alreadyFound))

    distribute :: ( Eq terminal, Hashable terminal
                  , Eq config, Hashable config )
               => Vector (HashMap terminal (HashSet config))
               -> HashMap terminal (Vector (HashSet config))
    distribute start = start
      & Vec.imap (\idx m -> mkSingleVec (Vec.length start) idx <$> m)
      & foldl' (M.unionWith $ Vec.zipWith S.union) M.empty
    mkSingleVec :: Int -> Int -> HashSet config -> Vector (HashSet config)
    mkSingleVec len idx configs =
      Vec.generate len (\idx' -> if idx == idx' then configs else S.empty)

    addTerminal :: ( Eq terminal, Hashable terminal
                   , Eq config, Hashable config )
                => HashMap [terminal] (HashMap terminal (Vector (HashSet config)))
                -> HashMap [terminal] (Vector (HashSet config))
    addTerminal start = M.fromListWith (Vec.zipWith S.union) $ do
      (word, m) <- M.toList start
      (terminal, m') <- M.toList m
      pure (terminal : word, m')

    isFinal :: NVA s sta i o c -> (s, [sta]) -> Bool
    isFinal NVA{final} (s, []) = S.member s final
    isFinal _ _ = False

mapKeys :: (Eq k2, Hashable k2) => (k1 -> k2) -> HashMap k1 v -> HashMap k2 v
mapKeys convert = M.toList >>> fmap (first convert) >>> M.fromList

asNFA :: forall s sta i o c.
         ( Eq s, Hashable s
         , Eq sta, Hashable sta
         , Eq i, Hashable i
         , Eq o, Hashable o
         , Eq c, Hashable c)
      => NVA s sta i o c -> N.NFA s (FakeEdge sta i o c)
asNFA NVA{..} = N.NFA
  { N.initial = pretendSingle initial
  , N.final = final
  , N.transitions = mapKey InnerEdge <$> innerTransitions
    & N.mergeTransitions (toMap Push <$> openTransitions)
    & N.mergeTransitions (toMap Pop <$> closeTransitions) }
  where
    mapKey f = M.toList >>> fmap (first f) >>> M.fromList
    toMap :: (Eq fakeEdge, Hashable fakeEdge) => (a -> sta -> fakeEdge) -> HashMap a (HashSet (sta, s)) -> HashMap fakeEdge (HashSet s)
    toMap f original = M.toList original
      >>= (\(a, stas) -> (\(sta, s) -> (f a sta, S.singleton s)) <$> toList stas)
      & M.fromListWith S.union
    pretendSingle = toList >>> \case
      [a] -> a
      as -> compErr "Data.Automaton.NVA.asNFA.pretendSingle" $ "We don't support pp for non-single-initial states, got " <> show (length as) <> " states."

data FakeEdge sta i o c = Push o sta | Pop c sta | InnerEdge i deriving (Eq, Show, Generic)
instance (Hashable sta, Hashable i, Hashable o, Hashable c) => Hashable (FakeEdge sta i o c)

ppFakeEdge :: (Show sta, Show i, Show o, Show c) => FakeEdge sta i o c -> Text
ppFakeEdge (Push o sta) = show o <> " +" <> show sta
ppFakeEdge (Pop c sta) = show c <> " -" <> show sta
ppFakeEdge (InnerEdge i) = show i
