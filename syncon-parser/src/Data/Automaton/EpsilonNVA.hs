{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Automaton.EpsilonNVA where

import Pre hiding (concat)

import qualified Data.HashSet as S
import qualified Data.HashMap.Lazy as M

import Data.Automaton (EpsNFA(EpsNFA))
import qualified Data.Automaton as EpsNFA

-- | A visibly pushdown automaton with
-- * 's' as the states
-- * 'sta' as the stack alphabet
-- * 'i' as the alphabet for inner transitions
-- * 'o' as the alphabet for call transitions (mnemonic "open")
-- * 'c' as the alphabet for return transitions (mnemonic "close")
data EpsNVA s sta i o c = EpsNVA
  { initial :: HashSet s
  , innerTransitions :: HashMap s (HashMap (Maybe i) (HashSet s))
  , openTransitions :: HashMap s (HashMap o (HashSet (sta, s)))
  , closeTransitions :: HashMap s (HashMap c (HashSet (sta, s)))
  , final :: HashSet s }
  deriving (Show)

data TaggedTerminal i o c
  = Open o
  | Inner i
  | Close c
  deriving (Eq, Generic, Show)
instance (Hashable i, Hashable o, Hashable c) => Hashable (TaggedTerminal i o c)

untag :: (o -> a) -> (i -> a) -> (c -> a) -> TaggedTerminal i o c -> a
untag f _ _ (Open o) = f o
untag _ f _ (Inner i) = f i
untag _ _ f (Close c) = f c

partitionTagged :: Foldable f => f (TaggedTerminal i o c) -> ([i], [o], [c])
partitionTagged = toList >>> recur
  where
    recur [] = ([], [], [])
    recur (Open o : rest) = let (is, os, cs) = recur rest in (is, o : os, cs)
    recur (Inner i : rest) = let (is, os, cs) = recur rest in (i : is, os, cs)
    recur (Close c : rest) = let (is, os, cs) = recur rest in (is, os, c : cs)

fromNFA :: ( Eq s, Hashable s
           , Eq i, Hashable i
           , Eq o, Hashable o
           , Eq c, Hashable c )
        => (a -> TaggedTerminal i o c) -> EpsNFA s a -> EpsNVA s () i o c
fromNFA tag EpsNFA{EpsNFA.initial, EpsNFA.transitions, EpsNFA.final} = EpsNVA
  { initial = S.singleton initial
  , innerTransitions = fromTriples iTrip
  , openTransitions = fromTriples oTrip
  , closeTransitions = fromTriples cTrip
  , final = final }
  where
    (iTrip, oTrip, cTrip) = toTriples transitions
      <&> (\(s1, ma, s2) ->
            case tag <$> ma of
              Nothing -> Inner (s1, Nothing, s2)
              Just (Inner i) -> Inner (s1, Just i, s2)
              Just (Open o) -> Open (s1, o, ((), s2))
              Just (Close c) -> Close (s1, c, ((), s2)))
      & partitionTagged

-- | Retrieve all the states mentioned in an EpsNVA
states :: (Eq s, Hashable s) => EpsNVA s sta i o c -> HashSet s
states EpsNVA{..} = initial `S.union` final `S.union` S.fromList (inners ++ opens ++ closes)
  where
    inners = toTriples innerTransitions
      & concatMap (\(s1, _, s2) -> [s1, s2])
    opens = toTriples openTransitions
      & concatMap (\(s1, _, (_, s2)) -> [s1, s2])
    closes = toTriples closeTransitions
      & concatMap (\(s1, _, (_, s2)) -> [s1, s2])

mapStates :: ( Eq s2, Hashable s2
             , Eq sta, Hashable sta
             , Eq i, Hashable i
             , Eq o, Hashable o
             , Eq c, Hashable c )
          => (s1 -> s2) -> EpsNVA s1 sta i o c -> EpsNVA s2 sta i o c
mapStates convert EpsNVA{..} = EpsNVA
  { initial = S.map convert initial
  , openTransitions = toTriples openTransitions
    & fmap (\(s1, o, (sta, s2)) -> (convert s1, o, (sta, convert s2)))
    & fromTriples
  , innerTransitions = toTriples innerTransitions
    & fmap (\(s1, i, s2) -> (convert s1, i, convert s2))
    & fromTriples
  , closeTransitions = toTriples closeTransitions
    & fmap (\(s1, c, (sta, s2)) -> (convert s1, c, (sta, convert s2)))
    & fromTriples
  , final = S.map convert final }

stackSymbols :: (Eq sta, Hashable sta) => EpsNVA s sta i o c -> HashSet sta
stackSymbols EpsNVA{openTransitions,closeTransitions} =
  foldMap (foldMap $ S.map fst) openTransitions <> foldMap (foldMap $ S.map fst) closeTransitions

mapSta :: (Eq s, Hashable s, Eq stb, Hashable stb)
       => (sta -> stb) -> EpsNVA s sta i o c -> EpsNVA s stb i o c
mapSta convert nva@EpsNVA{openTransitions, closeTransitions} = nva
  { openTransitions = fmap (S.map $ first convert) <$> openTransitions
  , closeTransitions = fmap (S.map $ first convert) <$> closeTransitions }

renumberStates :: forall s sta i o c.
                  ( Eq s, Hashable s
                  , Eq sta, Hashable sta
                  , Eq i, Hashable i
                  , Eq o, Hashable o
                  , Eq c, Hashable c )
               => EpsNVA s sta i o c -> EpsNVA Int sta i o c
renumberStates nva = mapStates oldToNew nva
  where
    oldToNew s = M.lookup s newStates
      & compFromJust "Data.Automaton.EpsilonNVA.renumberStates" "missing state"
    newStates :: HashMap s Int
    newStates = evalState computeNewStates 0
    computeNewStates = states nva & S.toMap & traverse (\_ -> get <* modify (+1))

renumberStack :: ( Eq s, Hashable s
                 , Eq sta, Hashable sta )
              => EpsNVA s sta i o c -> EpsNVA s Int i o c
renumberStack nva = mapSta oldToNew nva
  where
    oldToNew sta = M.lookup sta newSymbols
      & compFromJust "Data.Automaton.EpsilonNVA.renumberStack" "missing symbol"
    newSymbols = evalState computeNewSymbols 0
    computeNewSymbols = stackSymbols nva & S.toMap & traverse (\_ -> get <* modify (+1))

renumberStackKeepLeft :: ( Eq s, Hashable s
                         , Eq stal, Hashable stal
                         , Eq star, Hashable star )
                      => EpsNVA s (Either stal star) i o c -> EpsNVA s (Either stal Int) i o c
renumberStackKeepLeft nva = mapSta (second oldToNew) nva
  where
    oldToNew sta = M.lookup sta newSymbols
      & compFromJust "Data.Automaton.EpsilonNVA.renumberStackKeepLeft" "missing symbol"
    newSymbols = evalState computeNewSymbols 0
    computeNewSymbols = stackSymbols nva
      & toList
      & mapMaybe asRight
      & S.fromList
      & S.toMap
      & traverse (\_ -> get <* modify (+1))
    asRight Left{} = Nothing
    asRight (Right a) = Just a

renumber :: ( Eq s, Hashable s
            , Eq sta, Hashable sta
            , Eq i, Hashable i
            , Eq o, Hashable o
            , Eq c, Hashable c )
         => EpsNVA s sta i o c -> EpsNVA Int Int i o c
renumber = renumberStates >>> renumberStack

concat :: ( Eq s1, Hashable s1
          , Eq s2, Hashable s2
          , Eq sta1, Hashable sta1
          , Eq sta2, Hashable sta2
          , Eq i, Hashable i
          , Eq o, Hashable o
          , Eq c, Hashable c )
       => EpsNVA s1 sta1 i o c -> EpsNVA s2 sta2 i o c -> EpsNVA (Either s1 s2) (Either sta1 sta2) i o c
concat (mapStates Left >>> mapSta Left -> a) (mapStates Right >>> mapSta Right -> b) = EpsNVA
  { initial = initial a
  , final = final b
  , innerTransitions = innerTransitions a <> innerTransitions b
    & M.unionWith (M.unionWith S.union) (fromTriples bridges)
  , openTransitions = openTransitions a <> openTransitions b
  , closeTransitions = closeTransitions a <> closeTransitions b }
  where
    bridges = (, Nothing, ) <$> toList (final a) <*> toList (initial b)

instance
  ( Eq i, Hashable i
  , Eq o, Hashable o
  , Eq c, Hashable c )
  => Semigroup (EpsNVA Int Int i o c) where
  a <> b = concat a b & renumber

instance
  ( Eq i, Hashable i
  , Eq o, Hashable o
  , Eq c, Hashable c )
  => Monoid (EpsNVA Int Int i o c) where
  mempty = EpsNVA
    { initial = S.singleton 0
    , innerTransitions = M.empty
    , openTransitions = M.empty
    , closeTransitions = M.empty
    , final = S.singleton 0 }
  mappend = (<>)

instance
  ( Eq i, Hashable i
  , Eq o, Hashable o
  , Eq c, Hashable c
  , Eq sta, Hashable sta)
  => Semigroup (EpsNVA Int (Either sta Int) i o c) where
  a <> b = concat a b & renumberStates & mapSta (either (shuffle Left) (shuffle Right))
    & renumberStackKeepLeft
    where
      shuffle :: (Int -> Either Int Int) -> Either sta Int -> Either sta (Either Int Int)
      shuffle _ (Left sta) = Left sta
      shuffle constr (Right i) = Right $ constr i

instance
  ( Eq i, Hashable i
  , Eq o, Hashable o
  , Eq c, Hashable c
  , Eq sta, Hashable sta)
  => Monoid (EpsNVA Int (Either sta Int) i o c) where
  mempty = EpsNVA
    { initial = S.singleton 0
    , innerTransitions = M.empty
    , openTransitions = M.empty
    , closeTransitions = M.empty
    , final = S.singleton 0 }
  mappend = (<>)

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
