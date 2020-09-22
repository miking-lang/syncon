{-# OPTIONS_GHC -fno-warn-deprecations #-}

{-
Welcome to your custom Prelude
Export here everything that should always be in your library scope
For more info on what is exported by Protolude check:
https://github.com/sdiehl/protolude/blob/master/Symbols.md
-}
module Pre
( module Exports
, (<>)

, (>>>)
, (<<<)
, (&&&)
, (***)

, coerce

, compErr
, compFromJust

, Equal
, equal
, isEqual
, getEqual
, equalBy

, Compose(..)

, HashMap
, mapFromFoldable
, HashSet
, LiftingHashMap(..)

, foldMapM

, traceWrap
) where

import Protolude as Exports hiding ((<>), reader, sourceLine, sourceColumn, splitAt)

import Control.Arrow ((***), (>>>), (<<<), (&&&))
import Data.Semigroup ((<>))
import Data.Coerce (coerce)

import Data.HashMap.Lazy (HashMap)
import Data.HashSet (HashSet)
import qualified Data.HashMap.Lazy as M

import Data.Functor.Compose (Compose(..))

compErr :: Text -> Text -> a
compErr section = panic . mappend ("Compiler error[" <> section <> "]")

compFromJust :: Text -> Text -> Maybe a -> a
compFromJust _ _ (Just a) = a
compFromJust section message _ = compErr section message

data Equal a = EqualAll | EqualOne a | NotEqual deriving (Eq, Show, Generic)
instance NFData a => NFData (Equal a)
instance Hashable a => Hashable (Equal a)

equal :: a -> Equal a
equal = EqualOne

isEqual :: Equal a -> Bool
isEqual EqualAll{} = True
isEqual EqualOne{} = True
isEqual _ = False

getEqual :: Equal a -> Maybe a
getEqual (EqualOne a) = Just a
getEqual _ = Nothing

equalBy :: (Foldable f, Eq b) => (a -> b) -> f a -> Bool
equalBy f = foldMap (f >>> equal) >>> isEqual

-- | Make a 'HashMap' given a function to generate a key and a collection of values
mapFromFoldable :: (Eq b, Hashable b, Foldable t) => (a -> b) -> t a -> HashMap b [a]
mapFromFoldable getName = toList >>> fmap (getName &&& pure) >>> M.fromListWith (flip (<>))

{-# WARNING traceWrap "'traceWrap' remains in code" #-}
traceWrap :: (Show a, Monad m) => Text -> a -> m b -> m b
traceWrap t a m = traceM ("Entering " <> t <> " " <> show a) *> m <* traceM ("Exiting " <> t <> " " <> show a)

-- | Extend 'foldMap' to allow side effects.
--
-- Internally, this is implemented using a strict left fold. This is used for
-- performance reasons. It also necessitates that this function has a @Monad@
-- constraint and not just an @Applicative@ constraint. For more information,
-- see
-- <https://github.com/commercialhaskell/rio/pull/99#issuecomment-394179757>.
--
-- @since 0.1.3.0
foldMapM  -- NOTE: stolen from RIO.Prelude
  :: (Monad m, Monoid w, Foldable t)
  => (a -> m w)
  -> t a
  -> m w
foldMapM f = foldlM
  (\acc a -> do
    w <- f a
    return $! mappend acc w)
  mempty

newtype LiftingHashMap k a = LiftingHashMap { unliftHashMap :: HashMap k a }

instance (Eq k, Hashable k, Semigroup a) => Semigroup (LiftingHashMap k a) where
  LiftingHashMap l <> LiftingHashMap r = LiftingHashMap $ M.unionWith (<>) l r

instance (Eq k, Hashable k, Semigroup a) => Monoid (LiftingHashMap k a) where
  mempty = LiftingHashMap mempty

instance Eq a => Semigroup (Equal a) where
  NotEqual <> _ = NotEqual
  _ <> NotEqual = NotEqual
  EqualAll <> b = b
  a <> EqualAll = a
  e@(EqualOne a) <> EqualOne b = if a == b then e else NotEqual

instance Eq a => Monoid (Equal a) where
  mempty = EqualAll
  mappend = (<>)
