{-# LANGUAGE TemplateHaskell #-}

-- | This module describes a surface level language definition, i.e., what a user encounters,
--   as opposed to what later algorithms use.
module P2LanguageDefinition.Types where

import Pre

import Data.Data (Data)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import qualified Data.HashMap.Strict as M

import P1Lexing.Types (Range, Ranged, range)

-- TODO: make this something more fancy once we start namespacing things
newtype Name = Name Text deriving (Show, Eq, Hashable, Data, Typeable, NFData)
newtype TypeName = TypeName Text deriving (Show, Eq, Ord, Hashable, Data, Typeable, NFData)

-- | Names for subsections of syntax descriptions.
newtype SDName = SDName Text deriving (Show, Eq, Hashable, Data, Typeable)

data DefinitionFile = DefinitionFile
  { syncons :: !(HashMap Name Syncon)
  , syntaxTypes :: !(HashMap TypeName (Either SyntaxType TokenType))
  , forbids :: !(Seq Forbid)
  , precedences :: !PrecedenceMatrix
  , comments :: !(Seq Comment)
  , bracketKind :: !((Either Text TypeName) -> BracketKind)
  , groupings :: !(HashMap TypeName (Seq (Either Text TypeName, Either Text TypeName)))
  }

data BracketKind = OpenBracket | NonBracket | CloseBracket deriving (Show, Eq, Generic)
instance Hashable BracketKind

-- | A big sum type of all the top-level declarations
data Top
  = SyntaxTypeTop SyntaxType
  | TokenTypeTop TokenType
  | CommentTop Comment
  | SynconTop Syncon
  | ForbidTop Forbid
  | PrecedenceTop PrecedenceList
  | GroupingTop Grouping
  deriving (Show, Data, Typeable)

-- |
-- = Syncons

-- | A single syncon definition. Note that we will have some special handling for
--   operators (infix, prefix, postfix), where certain special names are used in
--   the syntax description in proper positions.
data Syncon = Syncon
  { s_name :: !Name
  , s_syntaxType :: !(Range, TypeName)
  , s_syntaxDescription :: !SyntaxDescription
  , s_range :: !Range
  } deriving (Show, Data, Typeable)

-- | A syntax type definition.
data SyntaxType = SyntaxType
  { st_name :: !TypeName
  , st_range :: !Range
  } deriving (Show, Data, Typeable)

-- | A token syntax type definition, including the regex that recognizes it
data TokenType = TokenType
  { t_name :: !TypeName
  , t_regex :: !(Range, Text)
  , t_range :: !Range
  } deriving (Show, Data, Typeable)

-- | A comment type declaration
data Comment = Comment
  { c_beginRegex :: !(Range, Text)
  , c_endRegex :: !(Range, Text)
  , c_range :: !Range
  } deriving (Show, Data, Typeable)

-- | A grouping declaration
data Grouping = Grouping
  { g_open :: !(Range, Either Text TypeName)
  , g_close :: !(Range, Either Text TypeName)
  , g_syntaxType :: !(Range, TypeName)
  , g_range :: !Range
  } deriving (Show, Data, Typeable)

-- |
-- == Syntax Descriptions

-- | A syntax description present in a syncon (or an operator)
data SyntaxDescription
  = SDSeq Range (Seq SyntaxDescription)
  | SDAlt Range (Seq SyntaxDescription)
  | SDRep Range Repetition SyntaxDescription
  | SDNamed Range SDName SyntaxDescription
  | SDSyTy Range TypeName
  | SDRec Range Rec
  | SDToken Range Text -- ^ A literal token, i.e., something written as a quoted string in a .syncon file
  deriving (Eq, Show, Data, Typeable)

-- | Recursion to the same syntax type, behaves exactly the same as just writing that syntax type,
-- except in precedence lists, where forbids are generated for recs.
data Rec = LRec | RRec | Rec deriving (Show, Data, Typeable, Eq, Generic)
instance Hashable Rec

data Repetition = RepStar | RepQuestion | RepPlus deriving (Eq, Show, Data, Typeable)

-- |
-- = Disambiguation

-- | In the syncon with the first name, forbid the syntax-type appearing in the
--   syntax description with the second name, from parsing as the syncon with
--   the third name (without parens surrounding it). This is the most basic
--   disambiguation tool, which all other disambiguations eventually translate to.
data Forbid
  = Forbid !Range !(Range, Name) !(Range, SDName) !(Range, Name)
  | ForbidRec !Range !(Range, Name) !(Range, Rec) !(Range, Name)
  deriving (Show, Data, Typeable)

-- | Make the (operator) syncons appearing earlier in the list have higher precedence
--   than those that appear later (i.e., total precedence among the mentioned syncons).
--
--   Syncons with the same precedence that have the same associativity will
--   associate together, while those of differing associativity will have
--   undefined relative precedence.
--
--   The last member is an exception list, the operators appearing in the same list
--   there will *not* have their relative precedence defined by this precedence list.
data PrecedenceList = PrecedenceList !Range !(Seq (Seq (OpOrNot, Name))) !(Seq (Seq Name))
  deriving (Show, Data, Typeable)
data OpOrNot = Op | NonOp deriving (Eq, Show, Data, Typeable, Generic)
instance Hashable OpOrNot

-- | A (sparse) matrix of precedences. Should only ever contain references to syncons
-- defined as operators. Internally is a map from (min a b, max a b) names to orderings
-- and sets of where each pair is defined.
newtype PrecedenceMatrix = PrecedenceMatrix (HashMap (Name, Name) (Ordering, HashSet Range))
  deriving (Show)

-- | A partial order function on syncons given a precedence matrix.
precCompare :: PrecedenceMatrix -> Name -> Name -> Maybe Ordering
precCompare (PrecedenceMatrix mat) (Name n1) (Name n2)
  | n1 == n2 = Just EQ
  | otherwise = fst <$> M.lookup (Name $ min n1 n2, Name $ max n1 n2) mat

-- | A complete disambiguation elaboration
type Elaboration = HashMap (Name, Either Rec SDName) (HashSet Name)

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
  range (SDAlt r _) = r
  range (SDRep r _ _) = r
  range (SDNamed r _ _) = r
  range (SDSyTy r _) = r
  range (SDRec r _) = r
  range (SDToken r _) = r

instance Ranged Forbid where
  range (Forbid r _ _ _) = r
  range (ForbidRec r _ _ _) = r

instance Ranged PrecedenceList where
  range (PrecedenceList r _ _) = r
