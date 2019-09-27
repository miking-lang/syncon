{-# LANGUAGE TypeFamilies, AllowAmbiguousTypes #-}

module Text.Earley.Forest.ParseTree where

import Prelude

-- | The operations required to construct a parse tree. Since the grammar
-- may be ambiguous, the parse result will be a parse *forest*, formatted as
-- a directed acyclic graph (assuming no infinite ambiguities). The type
-- for which an instance is defined should thus represent a single level in
-- the parse tree, unfixed style. Essentially, this means that each point
-- that would be a recursion point should instead be an occurrence of the
-- type parameter.
--
-- Note additionally that nullary productions will never contribute to the
-- parse tree produced.
--
-- Laws:
-- label l (a <> b) = label l a <> label l b
-- label l mempty = mempty
-- a <-> (b <-> c) = (a <-> b) <-> c
-- emptyInt <-> a = a
-- a <-> emptyInt = a
class ParseTree (nodeF :: * -> *) where
  type Tok nodeF :: *
  type InteriorF nodeF :: * -> *
  type IntLabel nodeF :: *
  type NodeLabel nodeF :: *

  token :: IntLabel nodeF -> Tok nodeF -> InteriorF nodeF r
  nodeLeaf :: IntLabel nodeF -> r -> InteriorF nodeF r
  label :: IntLabel nodeF -> InteriorF nodeF r -> InteriorF nodeF r
  node :: NodeLabel nodeF -> Maybe (Tok nodeF, Tok nodeF) -> InteriorF nodeF r -> nodeF r

  (<->) :: InteriorF nodeF r -> InteriorF nodeF r -> InteriorF nodeF r
  emptyInt :: InteriorF nodeF r
