module Result where

import Pre

-- | An 'Applicative' representing either a value or an accumulated collection of errors
data Result e a
  = Data a
  | Error e
  deriving (Functor, Foldable, Traversable, Show)

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
-- while the latter only sees error in the first computation.
instance Semigroup e => Monad (Result e) where
  return = pure
  (>>) = (*>)
  Data a >>= f = f a
  Error e >>= _ = Error e
