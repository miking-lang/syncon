{-# LANGUAGE TemplateHaskell #-}

-- | This module describes a surface level language definition, i.e., what a user encounters,
--   as opposed to what later algorithms use.
module P2LanguageDefinition.Types where

import Pre

import Data.Functor.Foldable.TH (makeBaseFunctor)

import P1Lexing.Types (Range, Ranged, range)

-- TODO: make this something more fancy once we start namespacing things
newtype Name = Name Text deriving (Show, Eq, Hashable)
newtype TypeName = TypeName Text deriving (Show, Eq, Hashable)

data DefinitionFile = DefinitionFile
  { syncons :: !(HashMap Name Syncon)
  , syntaxTypes :: !(HashMap TypeName (Either SyntaxType TokenType))
  , forbids :: !(Seq Forbid)
  , precedences :: !(Seq PrecedenceList)
  } deriving (Show)

-- |
-- = Syncons

-- | A single syncon definition. Note that we will have some special handling for
--   operators (infix, prefix, postfix), where certain special names are used in
--   the syntax description in proper positions.
data Syncon = Syncon
  { s_name :: !Name
  , s_syntaxType :: !TypeName
  , s_syntaxDescription :: !SyntaxDescription
  , s_range :: !Range
  } deriving (Show)

-- | A syntax type definition.
data SyntaxType = SyntaxType
  { st_name :: !TypeName
  , st_range :: !Range
  } deriving (Show)

-- | A token syntax type definition, including the regex that recognizes it
data TokenType = TokenType
  { t_name :: !TypeName
  , t_regex :: !Text
  , t_range :: !Range
  } deriving (Show)

-- | A comment type declaration
data Comment = Comment
  { c_regex :: !Text
  , c_range :: !Range
  } deriving (Show)

-- |
-- == Syntax Descriptions

-- | A syntax description present in a syncon (or an operator)
data SyntaxDescription
  = SDSeq Range (Seq SyntaxDescription)
  | SDRep Range Repetition SyntaxDescription
  | SDNamed Range SDName SyntaxDescription
  | SDSyTy Range TypeName
  | SDToken Range Text -- ^ A literal token, i.e., something written as a quoted string in a .syncon file
  deriving (Show)

data Repetition = RepStar | RepQuestion | RepPlus deriving (Show)

-- | Names for subsections of syntax descriptions. These include special names
--   for operator positions
data SDName
  = SDLeft | SDRight
  | SDName Text
  deriving (Show)

-- |
-- = Disambiguation

-- | In the syncon with the first name, forbid the syntax-type appearing in the
--   syntax description with the second name, from parsing as the syncon with
--   the third name (without parens surrounding it). This is the most basic
--   disambiguation tool, which all other disambiguations eventually translate to
data Forbid = Forbid !Range !Name !SDName !Name deriving (Show)

-- | Make the (operator) syncons appearing earlier in the list have higher precedence
--   than those that appear later (i.e., total precedence among the mentioned syncons).
--
--   Syncons with the same precedence that have the same associativity will
--   associate together, while those of differing associativity will have
--   undefined relative precedence.
--
--   The last member is an exception list, the operators appearing in the same list
--   there will *not* have their relative precedence defined by this precedence list.
data PrecedenceList = PrecedenceList !Range !(Seq (Seq Name)) !(Seq (Seq Name)) deriving (Show)

makeBaseFunctor ''SyntaxDescription

instance Ranged Syncon where
  range = s_range

instance Ranged SyntaxType where
  range = st_range

instance Ranged TokenType where
  range = t_range

instance Ranged Comment where
  range = c_range

instance Ranged SyntaxDescription where
  range (SDSeq r _) = r
  range (SDRep r _ _) = r
  range (SDNamed r _ _) = r
  range (SDSyTy r _) = r
  range (SDToken r _) = r

instance Ranged Forbid where
  range (Forbid r _ _ _) = r

instance Ranged PrecedenceList where
  range (PrecedenceList r _ _) = r
