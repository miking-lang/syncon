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
, equalBy

, Compose(..)

, HashMap
, HashSet
) where

import Protolude as Exports hiding ((<>), reader, sourceLine, sourceColumn)

import Control.Arrow ((***), (>>>), (<<<), (&&&))
import Data.Semigroup ((<>))
import Data.Coerce (coerce)

import Data.HashMap.Lazy (HashMap)
import Data.HashSet (HashSet)

import Data.Functor.Compose (Compose(..))

compErr :: Text -> Text -> a
compErr section = panic . mappend ("Compiler error[" <> section <> "]")

compFromJust :: Text -> Text -> Maybe a -> a
compFromJust _ _ (Just a) = a
compFromJust section message _ = compErr section message

data Equal a = Equal (Maybe a) | NotEqual deriving (Eq, Show)

equal :: a -> Equal a
equal = Just >>> Equal

isEqual :: Equal a -> Bool
isEqual Equal{} = True
isEqual _ = False

equalBy :: (Foldable f, Eq b) => (a -> b) -> f a -> Bool
equalBy f = foldMap (f >>> equal) >>> isEqual

instance Eq a => Semigroup (Equal a) where
  NotEqual <> _ = NotEqual
  _ <> NotEqual = NotEqual
  Equal Nothing <> b = b
  a <> Equal Nothing = a
  e@(Equal (Just a)) <> Equal (Just b) = if a == b then e else NotEqual

instance Eq a => Monoid (Equal a) where
  mempty = Equal Nothing
  mappend = (<>)
