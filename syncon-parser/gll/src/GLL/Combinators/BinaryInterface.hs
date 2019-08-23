{-# LANGUAGE TypeOperators, FlexibleInstances #-}

-- |
-- This module provides the same functions and combinators as "GLL.Combinators.Interface".
-- The only difference is that the combinators of this module construct only symbol expressions ('SymbExpr'/'BNF').
-- The combinators are therefore easier to use: they are just as freely combined but with simpler types and simpler type-errors.
-- However, the the underlying grammars are binarised, resulting in slower parsing.
module GLL.Combinators.BinaryInterface (
    -- * Elementary parsers
    term_parser, satisfy,
    -- ** Elementary parsers using the 'Token' datatype 
    keychar, keyword, int_lit, float_lit, bool_lit, char_lit, string_lit, alt_id_lit, id_lit, token,
    -- ** Elementary character-level parsers
    char, 
    -- * Elementary combinators
    -- *** Sequencing
    (<**>),
    -- *** Choice
    (<||>),
    -- *** Semantic actions
    (<$$>),
    -- *** Nonterminal introduction
    (<:=>),(<::=>),chooses,chooses_prec,
    -- * Types
    -- ** Grammar (combinator expression) types
    BNF, SymbExpr, toSymb, mkRule,
    -- ** Parseable token types 
    Token(..), Parseable(..), SubsumesToken(..), unlexTokens, unlexToken,  
    -- * Running a parser 
    grammarOf, parse, printParseData, evaluatorWithParseData,
    -- **  Running a parser with options
    parseWithOptions, parseWithParseOptions, printEvalDataWithOptions, printParseDataWithOptions, evaluatorWithParseDataAndOptions,printGrammarData,
    -- *** Possible options
    CombinatorOptions, CombinatorOption, 
             GLL.Combinators.Options.maximumErrors, throwErrors, 
             maximumPivot, maximumPivotAtNt, leftBiased,
    -- **** Parser options
    fullSPPF, allNodes, packedNodesOnly, strictBinarisation, 
      GLL.Parser.noSelectTest,
    -- *** Running a parser with options and explicit failure
    parseWithOptionsAndError, parseWithParseOptionsAndError,
    -- ** Runing a parser to obtain 'ParseResult'.
    parseResult, parseResultWithOptions,ParseResult(..),
    -- ** Builtin lexers.
    default_lexer, 
    -- *** Lexer settings
        lexer, LexerSettings(..), emptyLanguage,
    -- * Derived combinators
    mkNt, 
    -- *** Ignoring semantic results
    (<$$), (**>), (<**),
    -- *** EBNF patterns
    optional, preferably, reluctantly, optionalWithDef,
    multiple, multiple1, multipleSepBy, multipleSepBy1,
      multipleSepBy2, within, parens, braces, brackets, angles,
     -- *** Disambiguation  
            (<:=), (<::=),(<<<**>), (<**>>>), (<<**>), (<<<**), (**>>>), (<**>>),
            longest_match,shortest_match,
            many, many1, some, some1, 
            manySepBy, manySepBy1, manySepBy2, 
              someSepBy, someSepBy1,someSepBy2,
     -- * Memoisation
    memo, newMemoTable, memClear, MemoTable, MemoRef, useMemoisation,
    module GLL.Combinators.Interface
    ) where

import GLL.Combinators.Interface hiding (within, (**>), (<**>), (<**), (<<<**>), (<<<**), (**>>>), (<**>>>), satisfy, (<||>), (<||), (||>), (<$$>), (<$$), (<:=>), (<:=),(<::=>), (<::=), mkNt, manySepBy, manySepBy1, manySepBy2, multiple, multipleSepBy, many, multipleSepBy1, multipleSepBy2, someSepBy, someSepBy1, someSepBy2, some, memo, some1, many1, multiple1, shortest_match, longest_match, (<**>>), (<<**>), angles, braces, brackets, parens, within, optional, optionalWithDef, preferably, reluctantly, chooses, chooses_prec)
import qualified GLL.Combinators.Interface as IF
import GLL.Combinators.Options
import GLL.Combinators.Visit.Join
import GLL.Combinators.Visit.Sem (emptyAncestors)
import GLL.Combinators.Memoisation
import GLL.Combinators.Lexer
import GLL.Types.Grammar
import GLL.Parser hiding (parse, parseWithOptions)
import qualified GLL.Parser as GLL

import Control.Compose (OO(..))
import Control.Arrow
import qualified Data.Array as A
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Text (pack)
import Data.IORef 
import Data.Time.Clock
import System.IO.Unsafe


infixl 2 <:=>
-- | 
-- Form a rule by giving the name of the left-hand side of the new rule.
-- Use this combinator on recursive non-terminals.
(<:=>) :: (Show t, Ord t) => String -> BNF t a -> BNF t a 
n <:=> p = n IF.<:=> p
infixl 2 <::=>

-- | 
--  Variant of '<:=>' for recursive non-terminals that have a potentially infinite
--  number of derivations for some input string.
--
--  A non-terminal yields infinitely many derivations  
--  if and only if it is left-recursive and would be
--  left-recursive if all the right-hand sides of the productions of the
--  grammar are reversed.
(<::=>) :: (Show t, Ord t) => String -> BNF t a -> BNF t a
n <::=> p = n IF.<::=> p

-- | Variant of '<::=>' that can be supplied with a list of alternates
chooses :: (Show t, Ord t) => String -> [BNF t a] -> BNF t a
chooses p alts = IF.chooses p alts 

-- | Variant of '<::=' that can be supplied with a list of alternates
chooses_prec :: (Show t, Ord t) => String -> [BNF t a] -> BNF t a
chooses_prec p alts = IF.chooses_prec p alts 

infixl 4 <$$>
-- |
-- Form an 'AltExpr' by mapping some semantic action overy the result
-- of the second argument.
(<$$>) :: (Show t, Ord t) => (a -> b) -> BNF t a -> BNF t b
f <$$> p' = IF.toSymb (f IF.<$$> p')

infixl 4 <**>,<<<**>,<**>>>
-- | 
-- Add a 'SymbExpr' to the right-hand side represented by an 'AltExpr'
-- creating a new 'AltExpr'. 
-- The semantic result of the first argument is applied to the second 
-- as a cross-product. 
(<**>) :: (Show t, Ord t) =>  BNF t (a -> b) -> BNF t a -> BNF t b
pl' <**> pr' = IF.toSymb (pl' IF.<**> pr')

-- | Variant of '<**>' that applies longest match on the left operand.
(<**>>>) :: (Show t, Ord t) => BNF t (a -> b) -> BNF t a -> BNF t b
pl' <**>>> pr' = IF.toSymb (pl' IF.<**>>> pr')

-- | Variant of '<**>' that applies shortest match on the left operand.
(<<<**>) :: (Show t, Ord t) => BNF t (a -> b) -> BNF t a -> BNF t b
pl' <<<**> pr' = IF.toSymb (pl' IF.<<<**> pr')

infixr 3 <||>
-- |
-- Add an 'AltExpr' to a list of 'AltExpr'
-- The resuling  '[] :. AltExpr' forms the right-hand side of a rule.
(<||>) :: (Show t, Ord t) => BNF t a -> BNF t a -> BNF t a
l' <||> r' = IF.toSymb (l' IF.<||> r') 

-- |
-- Apply this combinator to an alternative to turn all underlying occurrences
-- of '<**>' (or variants) apply 'longest match'.
longest_match :: (Show t, Ord t) => BNF t a -> BNF t a
longest_match isalt = IF.toSymb (IF.longest_match isalt)

-- Apply this combinator to an alternative to turn all underlying occurrences
-- of '<**>' (or variants) apply 'shortest match'.
shortest_match :: (Show t, Ord t) => BNF t a -> BNF t a
shortest_match isalt = IF.toSymb (IF.shortest_match isalt)

-- | The empty right-hand side that yields its 
--  first argument as a semantic result.
satisfy :: (Show t, Ord t ) => a -> BNF t a
satisfy a = IF.toSymb (IF.satisfy a)

-- | 
-- This function memoises a parser, given:
--
-- * A 'MemoRef' pointing to a fresh 'MemoTable', created using 'newMemoTable'.
-- * The 'SymbExpr' to memoise.
--
-- Use 'memo' on those parsers that are expected to derive the same 
-- substring multiple times. If the same combinator expression is used
-- to parse multiple times the 'MemoRef' needs to be cleared using 'memClear'.
--
-- 'memo' relies on 'unsafePerformIO' and is therefore potentially unsafe.
-- The option 'useMemoisation' enables memoisation.
-- It is off by default, even if 'memo' is used in a combinator expression.
memo :: (Ord t, Show t) => MemoRef [a] -> BNF t a -> BNF t a
memo ref p' = IF.memo ref p' 
-- | 
-- Helper function for defining new combinators.
-- Use 'mkNt' to form a new unique non-terminal name based on
-- the symbol of a given 'SymbExpr' and a 'String' that is unique to
-- the newly defined combinator.
mkNt :: (Show t, Ord t) => BNF t a -> String -> String 
mkNt p str = IF.mkNt p str 

-- | 
-- Variant of '<$$>' that ignores the semantic result of its second argument. 
(<$$) :: (Show t, Ord t) => b -> BNF t a -> BNF t b
f <$$ p = const f <$$> p
infixl 4 <$$

-- | 
infixl 4 **>, <<**>, **>>>

-- | 
-- Variant of '<**>' that ignores the semantic result of the first argument.
(**>) :: (Show t, Ord t) => BNF t a -> BNF t b -> BNF t b
l **> r = flip const <$$> l <**> r

-- Variant of '<**>' that applies longest match on its left operand. 
(**>>>) :: (Show t, Ord t) => BNF t a -> BNF t b -> BNF t b
l **>>> r = flip const <$$> l <**>>> r

-- Variant of '<**>' that ignores shortest match on its left operand.
(<<**>) :: (Show t, Ord t) => BNF t a -> BNF t b -> BNF t b
l <<**>r = flip const <$$> l <<<**> r


infixl 4 <**, <<<**, <**>>
-- | 
-- Variant of '<**>' that ignores the semantic result of the second argument.
(<**) :: (Show t, Ord t) => BNF t a -> BNF t b -> BNF t a
l <** r = const <$$> l <**> r 

-- | Variant of '<**' that applies longest match on its left operand.
(<**>>) :: (Show t, Ord t) => BNF t a -> BNF t b -> BNF t a
l <**>> r = const <$$> l <**>>> r 

-- | Variant '<**' that applies shortest match on its left operand
(<<<**) :: (Show t, Ord t) => BNF t a -> BNF t b -> BNF t a
l <<<** r = const <$$> l <<<**> r 

-- | 
-- Variant of '<::=>' that prioritises productions from left-to-right (or top-to-bottom).
x <::= altPs = x IF.<::= altPs
infixl 2 <::=

-- | 
-- Variant of '<:=>' that prioritises productions from left-to-right (or top-to-bottom).
x <:= altPs = x IF.<:= altPs
infixl 2 <:=

-- | Try to apply a parser multiple times (0 or more) with shortest match
-- applied to each occurrence of the parser.
many :: (Show t, Ord t) => BNF t a -> BNF t [a]
many = multiple_ (<<<**>)

-- | Try to apply a parser multiple times (1 or more) with shortest match
-- applied to each occurrence of the parser.
many1 :: (Show t, Ord t) => BNF t a -> BNF t [a]
many1 = multiple1_ (<<<**>) 

-- | Try to apply a parser multiple times (0 or more) with longest match
-- applied to each occurrence of the parser.
some :: (Show t, Ord t) => BNF t a -> BNF t [a]
some = multiple_ (<**>>>)

-- | Try to apply a parser multiple times (1 or more) with longest match
-- applied to each occurrence of the parser.
some1 :: (Show t, Ord t) => BNF t a -> BNF t [a]
some1 = multiple1_ (<**>>>) 

-- | Try to apply a parser multiple times (0 or more). The results are returned in a list.
-- In the case of ambiguity the largest list is returned.
multiple :: (Show t, Ord t) => BNF t a -> BNF t [a]
multiple = multiple_ (<**>)

-- | Try to apply a parser multiple times (1 or more). The results are returned in a list.
-- In the case of ambiguity the largest list is returned.
multiple1 :: (Show t, Ord t) => BNF t a -> BNF t [a]
multiple1 = multiple1_ (<**>)

-- | Internal
multiple_ disa p = let fresh = mkNt p "*" 
                    in fresh <::=> ((:) <$$> p) `disa` (multiple_ disa p) <||> satisfy []

-- | Internal
multiple1_ disa p = let fresh = mkNt p "+"
                     in fresh <::=> ((:) <$$> p) `disa` (multiple_ disa p)

-- | Same as 'many' but with an additional separator.
manySepBy :: (Show t, Ord t) => BNF t a -> BNF t b -> BNF t [a]
manySepBy = sepBy many
-- | Same as 'many1' but with an additional separator.
manySepBy1 :: (Show t, Ord t) => BNF t a -> BNF t b -> BNF t [a]
manySepBy1 = sepBy1 many
-- | Same as 'some1' but with an additional separator.
someSepBy :: (Show t, Ord t) => BNF t a -> BNF t b -> BNF t [a]
someSepBy = sepBy some
-- | Same as 'some1' but with an additional separator.
someSepBy1 :: (Show t, Ord t) => BNF t a -> BNF t b -> BNF t [a]
someSepBy1 = sepBy1 some
-- | Same as 'multiple' but with an additional separator.
multipleSepBy :: (Show t, Ord t) => BNF t a -> BNF t b -> BNF t [a]
multipleSepBy = sepBy multiple 
-- | Same as 'multiple1' but with an additional separator.
multipleSepBy1 :: (Show t, Ord t) => BNF t a -> BNF t b -> BNF t [a]
multipleSepBy1 = sepBy1 multiple 

sepBy :: (Show t, Ord t) => (BNF t a -> BNF t [a]) -> BNF t a -> BNF t b -> BNF t [a]
sepBy mult p c = mkRule $ satisfy [] <||> (:) <$$> p <**> mult (c **> p)

sepBy1 :: (Show t, Ord t) => (BNF t a -> BNF t [a]) -> BNF t a -> BNF t b -> BNF t [a]
sepBy1 mult p c = mkRule $ (:) <$$> p <**> mult (c **> p)

-- | Like 'multipleSepBy1' but matching at least two occurrences of the 
-- first argument. The returned list is therefore always of at least
-- length 2. At least one separator will be consumed.
multipleSepBy2 p s = mkRule $
  (:) <$$> p <** s <**> multipleSepBy1 p s

-- | Like 'multipleSepBy2' but matching the minimum number of 
-- occurrences of the first argument as possible (at least 2).
someSepBy2 p s = mkRule $
  (:) <$$> p <** s <**> someSepBy1 p s

-- | Like 'multipleSepBy2' but matching the maximum number of
-- occurrences of the first argument as possible (at least 2).
manySepBy2 p s = mkRule $ 
  (:) <$$> p <** s <**> manySepBy1 p s

-- | Derive either from the given symbol or the empty string.
optional :: (Show t, Ord t) => BNF t a -> BNF t (Maybe a)
optional p = fresh 
  <:=>  Just <$$> p 
  <||>  satisfy Nothing 
  where fresh = mkNt p "?"

-- | Version of 'optional' that prefers to derive from the given symbol,
-- affects only nullable nonterminal symbols
preferably :: (Show t, Ord t) => BNF t a -> BNF t (Maybe a)
preferably p = fresh 
  <:=   Just <$$> p 
  <||>  satisfy Nothing 
  where fresh = mkNt p "?"

-- | Version of 'optional' that prefers to derive the empty string from 
-- the given symbol, affects only nullable nonterminal symbols
reluctantly :: (Show t, Ord t) => BNF t a -> BNF t (Maybe a)
reluctantly p = fresh 
  <:=   satisfy Nothing  
  <||>  Just <$$> p
  where fresh = mkNt p "?"

optionalWithDef :: (Show t, Ord t) => BNF t a -> a -> BNF t a 
optionalWithDef p def = mkNt p "?" <:=> id <$$> p <||> satisfy def

-- | Place a piece of BNF /within/ two other BNF fragments, ignoring their semantics.
within :: (Show t, Ord t) => BNF t a -> BNF t b -> BNF t c -> BNF t b
within l p r = IF.toSymb (l **> p <** r)

-- | Place a piece of BNF between the characters '(' and ')'.
parens p = within (keychar '(') p (keychar ')')
-- | Place a piece of BNF between the characters '{' and '}'.
braces p = within (keychar '{') p (keychar '}')
-- | Place a piece of BNF between the characters '[' and ']'.
brackets p = within (keychar '[') p (keychar ']')
-- | Place a piece of BNF between the characters '<' and '>'.
angles p = within (keychar '<') p (keychar '>')
-- | Place a piece of BNF between two single quotes.
quotes p = within (keychar '\'') p (keychar '\'')
-- | Place a piece of BNF between two double quotes.
dquotes p = within (keychar '"') p (keychar '"')
