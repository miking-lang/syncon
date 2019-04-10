{-# LANGUAGE TemplateHaskell #-}

-- | This module describes a surface level language definition, i.e., what a user encounters,
--   as opposed to what later algorithms use.
module P2LanguageDefinition.Types where

import Pre

import Data.Functor.Foldable.TH (makeBaseFunctor)

import P1Lexing.Types (Range, Ranged, range)
import qualified P1Lexing.Types as P1

-- TODO: make this something more fancy once we start namespacing things
newtype Name = Name Text

-- |
-- = Syncons

-- | A single syncon definition. Note that we will have some special handling for
--   operators (infix, prefix, postfix), where certain special names are used in
--   the syntax description in proper positions.
data Syncon = Syncon
  { s_name :: Name
  , s_syntaxType :: Name
  , s_syntaxDescription :: SyntaxDescription
  , s_range :: Range
  }

-- |
-- == Syntax Descriptions

-- | A syntax description present in a syncon (or an operator)
data SyntaxDescription
  = SDSeq Range (Seq SyntaxDescription)
  | SDRep Range Repetition SyntaxDescription
  | SDNamed Range SDName SyntaxDescription
  | SDToken P1.Token -- ^ A literal token, i.e., something written as a quoted string in a .syncon file

data Repetition = RepStar | RepQuestion | RepPlus

-- | Names for subsections of syntax descriptions. These include special names
--   for operator positions
data SDName
  = SDBinaryLeft | SDBinaryRight
  | SDUnary
  | SDName Text

-- |
-- = Disambiguation

-- | In the syncon with the first name, forbid the syntax-type appearing in the
--   syntax description with the second name, from parsing as the syncon with
--   the third name (without parens surrounding it). This is the most basic
--   disambiguation tool, which all other disambiguations eventually translate to
data Forbid = Forbid Range Name SDName Name

-- | Make the (operator) syncons appearing earlier in the list have higher precedence
--   than the latter (i.e., total precedence among the mentioned syncons).
--
--   Syncons with the same precedence that have the same associativity will
--   associate together, while those of differing associativity will have
--   undefined relative precedence.
data PrecedenceList = PrecedenceList Range (Seq (Seq Name))

makeBaseFunctor ''SyntaxDescription

instance Ranged Syncon where
  range = s_range

instance Ranged SyntaxDescription where
  range (SDSeq r _) = r
  range (SDRep r _ _) = r
  range (SDNamed r _ _) = r
  range (SDToken tok) = range tok

instance Ranged Forbid where
  range (Forbid r _ _ _) = r

instance Ranged PrecedenceList where
  range (PrecedenceList r _) = r