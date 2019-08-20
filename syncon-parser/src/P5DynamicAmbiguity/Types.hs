module P5DynamicAmbiguity.Types where

import Pre

import P1Lexing.Types (Ranged(..))
import qualified P2LanguageDefinition.Types as P2
import qualified P4Parsing.Types as P4

data NodeOrElide elidable
  = Node !(P4.NodeF P4.SingleLanguage P2.TypeName (NodeOrElide elidable))
  | Elide !elidable
  deriving (Show)

data Token elidable
  = LitTok !Text
  | OtherTokInstance !P2.TypeName !Text
  | OtherTok !P2.TypeName
  | ElidedTok !elidable
  deriving (Show)

eitherRepr :: Token elidable -> Either Text (Either P2.TypeName elidable)
eitherRepr (LitTok t) = Left t
eitherRepr (OtherTokInstance n _) = Right $ Left n
eitherRepr (OtherTok n) = Right $ Left n
eitherRepr (ElidedTok e) = Right $ Right e

textualToken :: (elidable -> Text) -> Token elidable -> Text
textualToken _ (LitTok t) = t
textualToken _ (OtherTokInstance _ t) = t
textualToken _ (OtherTok tyn) = coerce tyn  -- TODO: make this more distinct somehow
textualToken showElide (ElidedTok e) = showElide e

mkTok :: Either Text P2.TypeName -> Token elidable
mkTok (Left t) = LitTok t
mkTok (Right n) = OtherTok n

instance Eq elidable => Eq (Token elidable) where
  (==) = (==) `on` eitherRepr
instance Hashable elidable => Hashable (Token elidable) where
  hashWithSalt = hashUsing eitherRepr

instance Ranged elidable => Ranged (NodeOrElide elidable) where
  range (Node n) = range n
  range (Elide e) = range e
