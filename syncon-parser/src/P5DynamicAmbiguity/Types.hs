{-# LANGUAGE TemplateHaskell #-}

module P5DynamicAmbiguity.Types where

import Pre

import Codec.Serialise (Serialise)

import P1Lexing.Types (Ranged(..))
import qualified P1Lexing.Types as P1
import qualified P2LanguageDefinition.Types as P2
import qualified P4Parsing.Types as P4

data NodeOrElide elidable t
  = Node !(P4.NodeF t (NodeOrElide elidable t))
  | Elide !elidable
  deriving (Show, Generic, Eq)

countNodesInNodeOrElide :: NodeOrElide elidable t -> Int
countNodesInNodeOrElide = recur >>> getSum
  where
    recur (Node n) = Sum 1 <> foldMap recur n
    recur (Elide _) = Sum 1

data Token elidable
  = LitTok !Text
  | OtherTokInstance !P2.TypeName !Text
  | OtherTok !P2.TypeName
  | ElidedTok !elidable
  deriving (Show, Generic)
instance Serialise elidable => Serialise (Token elidable)

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

convertToken :: P1.Token l P2.TypeName -> Token elidable
convertToken (P1.LitTok _ _ t) = LitTok t
convertToken (P1.OtherTok _ _ tyn t) = OtherTokInstance tyn t

makeFakeToken :: l -> (elidable -> P1.Token l P2.TypeName) -> Token elidable -> P1.Token l P2.TypeName
makeFakeToken l _ (LitTok t) = P1.LitTok mempty l t
makeFakeToken l _ (OtherTokInstance tyn t) = P1.OtherTok mempty l tyn t
makeFakeToken l _ (OtherTok tyn) = P1.OtherTok mempty l tyn ("<" <> show tyn <> ">")
makeFakeToken _ fromElide (ElidedTok e) = fromElide e

instance Eq elidable => Eq (Token elidable) where
  (==) = (==) `on` eitherRepr
instance Hashable elidable => Hashable (Token elidable) where
  hashWithSalt = hashUsing eitherRepr

instance Ranged elidable => Ranged (NodeOrElide elidable t) where
  range (Node n) = range n
  range (Elide e) = range e
instance (Hashable elidable, Hashable t) => Hashable (NodeOrElide elidable t)
