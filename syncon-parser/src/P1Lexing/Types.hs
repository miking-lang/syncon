module P1Lexing.Types where

import Pre

import Data.Data (Data)
import Codec.Serialise (Serialise)

import Text.Earley.Forest.Grammar as Forest

import P4Parsing.ForestParser (Unlexable(..))

data Range = Nowhere | Range !Text !Position !Position deriving (Show, Eq, Ord, Data, Typeable, Generic)
data Position = Position { line :: !Int, column :: !Int } deriving (Show, Eq, Ord, Data, Typeable, Generic)

firstPosition :: Position
firstPosition = Position { line = 1, column = 1 }

stepPosition :: Position -> Char -> Position
stepPosition (Position l _) '\n' = Position (l+1) (column firstPosition)
stepPosition (Position l c) _ = Position l (c+1)

textualRange :: Range -> Text
textualRange Nowhere = "(nowhere"
textualRange (Range f (Position l1 c1) (Position l2 c2))
  | l1 == l2 && c1 == c2 = f <> ":" <> show l1 <> ":" <> show c1
  | l1 == l2 = f <> ":" <> show l1 <> ":" <> show c1 <> "-" <> show c2
  | otherwise = f <> ":" <> show l1 <> ":" <> show c1 <> "-" <> show l2 <> ":" <> show c2

instance Semigroup Range where
  r1@(Range f1 s1 e1) <> r2@(Range f2 s2 e2)
    | f1 == f2 = Range f1 (min s1 s2) (max e1 e2)
    | f1 > f2 = r1
    | otherwise = r2
  Nowhere <> r = r
  r <> Nowhere = r
instance Monoid Range where
  mempty = Nowhere
  mappend = (<>)

-- | A token in language 'l', with tokenkind 'n' or literal
data Token l n
  = LitTok Range l Text
  | OtherTok Range l n Text
  deriving (Show, Eq, Ord, Generic, Data, Typeable)
instance (Hashable l, Hashable n) => Hashable (Token l n)
instance (NFData l, NFData n) => NFData (Token l n)

textualToken :: Show n => Token l n -> Text
textualToken (LitTok _ _ t) = show t
textualToken (OtherTok _ _ n t) = "(" <> show n <> ") " <> t  -- TODO: better printing of this

instance Coercible n Text => Forest.Parseable (Token l n) where  -- TODO: have some better method to print the 'n'
  type TokKind (Token l n) = TokenKind n
  kindLabel (LitKind t) = t
  kindLabel (TypeKind tyn) = coerce tyn
  getKind (LitTok _ _ t) = LitKind t
  getKind (OtherTok _ _ tyn _) = TypeKind tyn
  unlex (LitTok _ _ t) = t
  unlex (OtherTok _ _ _ t) = t

data TokenKind n = LitKind !Text | TypeKind !n
  deriving (Show, Eq, Generic)
instance Hashable n => Hashable (TokenKind n)
instance NFData n => NFData (TokenKind n)
instance Serialise n => Serialise (TokenKind n)

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

instance Unlexable (Token l n) where
  unlex (LitTok _ _ t) = t
  unlex (OtherTok _ _ _ t) = t

instance Hashable Range
instance Hashable Position
instance NFData Position
instance NFData Range
instance Serialise Position
instance Serialise Range
