module Result where

import Pre

import Data.Bitraversable (Bitraversable)
import Data.Bifoldable (Bifoldable(..))

-- | An 'Applicative' representing either a value or an accumulated collection of errors
data Result e a
  = Data a
  | Error e
  deriving (Functor, Foldable, Traversable, Show)

-- | Convenience function that makes an error if the parameter is
-- distinct from 'mempty' (which is expected to represent "no error").
errorIfNonEmpty :: (Monoid e, Eq e) => e -> Result e ()
errorIfNonEmpty e
  | e == mempty = Data ()
  | otherwise = Error e

instance Semigroup e => Applicative (Result e) where
  pure = Data
  Data f <*> Data a = Data $ f a
  Error e1 <*> Error e2 = Error $ e1 <> e2
  Error e <*> _ = Error e
  _ <*> Error e = Error e

-- | This instance is technically unlawful, though the effect will matter extremely rarely.
-- The law that is broken is the following:
-- > (<*>) = ap
-- This is not true since the former will collect errors from both the left and right computation,
-- while the latter only sees errors in the first computation.
instance Semigroup e => Monad (Result e) where
  return = pure
  (>>) = (*>)
  Data a >>= f = f a
  Error e >>= _ = Error e

instance Bifunctor Result where
  bimap f _ (Error e) = Error $ f e
  bimap _ f (Data a) = Data $ f a

instance Bifoldable Result where
  bifoldMap f _ (Error e) = f e
  bifoldMap _ f (Data a) = f a

instance Bitraversable Result

instance (Semigroup e, Semigroup a) => Semigroup (Result e a) where
  (<>) = liftA2 (<>)

instance (Semigroup e, Monoid a) => Monoid (Result e a) where
  mempty = Data mempty
  mappend = (<>)
