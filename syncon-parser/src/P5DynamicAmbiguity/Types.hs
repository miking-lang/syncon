module P5DynamicAmbiguity.Types where

import Pre

import qualified P2LanguageDefinition.Types as P2

data Token
  = LitTok Text
  | OtherTokInstance P2.TypeName Text
  | OtherTok P2.TypeName
  deriving (Show)

eitherRepr :: Token -> Either Text P2.TypeName
eitherRepr (LitTok t) = Left t
eitherRepr (OtherTokInstance n _) = Right n
eitherRepr (OtherTok n) = Right n

textualToken :: Token -> Text
textualToken (LitTok t) = t
textualToken (OtherTokInstance _ t) = t
textualToken (OtherTok tyn) = coerce tyn  -- TODO: make this more distinct somehow

mkTok :: Either Text P2.TypeName -> Token
mkTok (Left t) = LitTok t
mkTok (Right n) = OtherTok n

instance Eq Token where
  (==) = (==) `on` eitherRepr
instance Hashable Token where
  hashWithSalt = hashUsing eitherRepr
