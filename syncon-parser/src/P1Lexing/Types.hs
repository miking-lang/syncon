module P1Lexing.Types where

import Pre

data Range = Nowhere | Range Position Position
data Position = Position { line :: Int, column :: Int } deriving (Eq, Ord)

instance Semigroup Range where
  Range s1 e1 <> Range s2 e2 = Range (min s1 s2) (max e1 e2)
  Nowhere <> r = r
  r <> Nowhere = r
instance Monoid Range where
  mempty = Nowhere
  mappend = (<>)

data Token
  = IntTok Range Int
  | FloatTok Range Double
  | StringTok Range Text
  | IdentTok Range Text
  | OtherTok Range Text

class Ranged a where
  range :: a -> Range

instance Ranged Range where
  range = identity

instance Ranged Token where
  range (IntTok r _) = r
  range (FloatTok r _) = r
  range (StringTok r _) = r
  range (IdentTok r _) = r
  range (OtherTok r _) = r
