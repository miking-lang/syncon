module P1Lexing.Types where

import Pre

import Data.Data (Data)

data Range = Nowhere | Range !Position !Position deriving (Show, Eq, Data, Typeable, Generic)
data Position = Position { line :: !Int, column :: !Int } deriving (Show, Eq, Ord, Data, Typeable, Generic)

firstPosition :: Position
firstPosition = Position { line = 1, column = 1 }

instance Semigroup Range where
  Range s1 e1 <> Range s2 e2 = Range (min s1 s2) (max e1 e2)
  Nowhere <> r = r
  r <> Nowhere = r
instance Monoid Range where
  mempty = Nowhere
  mappend = (<>)

-- | A token in language 'l', with tokenkind 'n' or literal
data Token l n
  = LitTok Range l Text
  | OtherTok Range l n Text
  deriving (Show, Eq, Generic)
instance (Hashable l, Hashable n) => Hashable (Token l n)

tokenText :: Token l n -> Text
tokenText (LitTok _ _ t) = t
tokenText (OtherTok _ _ _ t) = t

class Ranged a where
  range :: a -> Range

instance Ranged Range where
  range = identity

instance (Ranged l, Ranged r) => Ranged (Either l r) where
  range = either range range

instance Ranged (a, Range) where
  range = snd

instance Ranged (Token l n) where
  range (LitTok r _ _) = r
  range (OtherTok r _ _ _) = r

instance Hashable Range
instance Hashable Position
