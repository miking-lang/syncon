{-# LANGUAGE DeriveFunctor, DeriveFoldable, ViewPatterns #-}

module Ambiguity (ambiguities, Repr) where

import Data.List (transpose)
import Data.Foldable (toList)
import qualified Data.Set as S
import Data.Function ((&))
import Control.Arrow ((>>>), (&&&))
import Control.Monad (void)

import Types.Lexer (Ranged(..), Range, Token)
import Types.Construction (Repeat(..))
import Types.Ast (NodeI(..), MidNodeI(..), FixNode(..))

type Node s = FixNode s String
type Unwrapped s = NodeI (s (Node s)) String

type Repr = (String, [String])

ambiguities :: [Node s] -> [(Range, S.Set Repr)]
ambiguities = fmap unSplice >>> ambiguities'

ambiguities' :: [Unwrapped s] -> [(Range, S.Set Repr)]
ambiguities' forest
  | equalBy (project >>> void) forest
  , all (equalBy range) subforests
    = subforests >>= ambiguities'
  | otherwise = [(range forest, S.fromList $ toRepr <$> forest)]
    where
      subforests = (project >>> toList) <$> forest & transpose
      toRepr = project >>> fmap (project >>> getTag) >>> getTag &&& toList
      getTag NodeF{name} = name
      getTag SyntaxSpliceF = "_splice_"

data NodeF r = NodeF
  { name :: String
  , children :: [MidNodeF r]
  , nodeRange :: Range }
  | SyntaxSpliceF
  deriving (Eq, Ord, Functor, Foldable, Show)
data MidNodeF r = MidNodeF Range r
                | MidIdentifierF Range String
                | MidSpliceF
                | BasicF Token
                | RepeatedF Range Repeat [MidNodeF r]
                | SequencedF Range [MidNodeF r]
                deriving (Eq, Ord, Functor, Foldable, Show)

project :: Unwrapped s -> NodeF (Unwrapped s)
project Node{name,children,nodeRange} =
  NodeF {name, children = projectMid <$> children, nodeRange}
  where
    projectMid (MidNode n) = MidNodeF (range n) n
    projectMid (MidIdentifier r i) = MidIdentifierF r i
    projectMid (MidSplice _) = MidSpliceF
    projectMid (Basic t) = BasicF t
    projectMid (Repeated rep cs) = RepeatedF (range cs) rep $ projectMid <$> cs
    projectMid (Sequenced r cs) = SequencedF r $ projectMid <$> cs
project SyntaxSplice{} = SyntaxSpliceF

equalBy :: Eq b => (a -> b) -> [a] -> Bool
equalBy f [] = True
equalBy f (a : as) = all (f >>> (== (f a))) as
