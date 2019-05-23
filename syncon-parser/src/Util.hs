{-# LANGUAGE ViewPatterns #-}

module Util where

import Pre hiding (all, check)

import qualified Data.HashSet as S
import qualified Data.HashMap.Lazy as M

-- Only run the action for new elements of the set
iterateInductivelyOptM :: forall s m. (Eq s, Hashable s, Monad m)
                       => (s -> m (HashSet s)) -> HashSet s -> m (HashSet s)
iterateInductivelyOptM f s = recur S.empty s
  where
    recur :: HashSet s -> HashSet s -> m (HashSet s)
    recur prev new
      | S.null new = return prev
      | otherwise = do
          nextStates <- traverse f $ S.toList new
          let newStates = S.unions nextStates `S.difference` prev
          recur (new <> prev) newStates

-- Run the action for every element in every iteration
iterateInductivelyM :: forall s m. (Eq s, Hashable s, Monad m)
                    => (s -> m (HashSet s)) -> HashSet s -> m (HashSet s)
iterateInductivelyM f s = recur S.empty s
  where
    recur :: HashSet s -> HashSet s -> m (HashSet s)
    recur prev new
      | S.null new = return prev
      | otherwise = do
          nextStates <- traverse f $ S.toList prev
          let newStates = S.unions nextStates `S.difference` prev
          recur (new <> prev) newStates

iterateInductively :: (Eq s, Hashable s) => (s -> HashSet s) -> HashSet s -> HashSet s
iterateInductively f s = runIdentity $ iterateInductivelyOptM (f >>> return) s

repeatUntilStableM :: (Monad m, Eq a) => m a -> m a
repeatUntilStableM action = action >>= recur
  where
    recur prev = action >>= \new -> if prev == new then return new else recur new

repeatUntilStableBy :: Eq b => (a -> b) -> (a -> a) -> a -> a
repeatUntilStableBy check step a = recur (check a) a
  where
    recur prev (step -> a')
      | prev == next = a'
      | otherwise = recur next a'
      where
        next = check a'

flipMap :: forall a b c. (Eq a, Hashable a, Eq b, Hashable b, Eq c, Hashable c)
        => HashMap a (HashMap b c)
        -> HashMap c (HashMap b (HashSet a))
flipMap = M.toList
  >>> concatMap toTriples
  >>> fmap rearrange
  >>> M.fromListWith (M.unionWith S.union)
  where
    toTriples (a, bc) = repeat a `zip` M.toList bc
    rearrange (a, (b, c)) = (c, M.singleton b $ S.singleton a)

mapBy :: (Eq k, Hashable k, Foldable f, Eq a, Hashable a)
      => (a -> k) -> f a -> HashMap k (HashSet a)
mapBy f = toList >>> fmap (f &&& S.singleton) >>> M.fromListWith S.union

-- TODO: something nice to index into maps and sets, essentially variadrically (this is lenses, isn't it)
-- TODO: make a monad for more specializable inductive iteration
