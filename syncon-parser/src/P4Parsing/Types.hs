{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module P4Parsing.Types where

import Pre

import Data.Data (Data)

import Data.Functor.Foldable.TH (makeBaseFunctor)

import P1Lexing.Types (Range, Ranged(..))
import qualified P1Lexing.Types as P1
import qualified P2LanguageDefinition.Types as P2

data Node l n = Node
  { n_name :: P2.Name
  , n_contents :: HashMap P2.SDName (Seq (NodeInternals l n (Node l n)))
  , n_range :: Range
  } deriving (Show, Eq, Data, Typeable)

data NodeInternals l n node
  = NodeLeaf node
  | TokenLeaf (P1.Token l n)
  | Struct (HashMap P2.SDName (Seq (NodeInternals l n node)))
  deriving (Show, Functor, Foldable, Traversable, Eq, Data, Typeable)

makeBaseFunctor ''Node
makeBaseFunctor ''NodeInternals
deriving instance (Eq l, Eq n, Eq a) => Eq (NodeF l n a)
deriving instance (Show l, Show n, Show a) => Show (NodeF l n a)

-- | Datatype to give the lexer to convince it to only parse a single language.
data SingleLanguage = SingleLanguage deriving (Show, Eq, Ord, Generic, Data, Typeable)
instance Hashable SingleLanguage
type SL = SingleLanguage

instance Ranged (Node l n) where
  range = n_range
instance Ranged (NodeF l n x) where
  range = n_rangeF

instance (Hashable l, Hashable n) => Hashable (Node l n) where
  hashWithSalt = hashUsing $ \(Node n c r) ->
    (n, toList <$> c, r)
instance (Hashable l, Hashable n, Hashable node) => Hashable (NodeF l n node) where
  hashWithSalt = hashUsing $ \(NodeF n c r) ->
    (n, toList <$> c, r)

instance (Hashable l, Hashable n, Hashable node) => Hashable (NodeInternals l n node) where
  hashWithSalt s (NodeLeaf n) = s `hashWithSalt` (0::Int) `hashWithSalt` n
  hashWithSalt s (TokenLeaf t) = s `hashWithSalt` (1::Int) `hashWithSalt` t
  hashWithSalt s (Struct m) = s `hashWithSalt` (2::Int) `hashWithSalt` (toList <$> m)
