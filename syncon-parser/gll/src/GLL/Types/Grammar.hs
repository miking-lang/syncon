{-# LANGUAGE StandaloneDeriving #-}


-- UUAGC 0.9.52.1 (src/GLL/Types/Abstract.ag)
module GLL.Types.Grammar where

import Prelude

import Data.Text

-- | Identifier for nonterminals.
type Nt  = Text
-- Prod ---------------------------------------------------------

-- |
-- A production binds a nonterminal identifier (left-hand side) to a list of symbols
--(the right-hand side of the production).
data Prod t = Prod (Nt) (Symbols t)
-- Prods --------------------------------------------------------
-- | A list of 'Prod's.
type Prods t = [Prod t]
-- Grammar -----------------------------------------------------
-- |
-- A grammar is a start symbol and a list of productions.
type Grammar t = (Nt, Prods t)
-- Slot --------------------------------------------------------
-- |
-- A grammar slot acts as a label to identify progress of matching a production.
-- As such, a slot is a "Prod" with its right-hand side split in two:
-- a part before and a part after 'the dot'.
-- The dot indicates which part of the right-hand side has been processed thus far.
data Slot t = Slot (Nt) (([Symbol t])) (([Symbol t]))
-- Symbol ------------------------------------------------------

-- |
-- A 'Symbol' is either a nonterminal or a terminal,
-- where a terminal contains some arbitrary token.
data Symbol t   = Nt Nt
                | Term t
--                | Error (Token) (Token)
-- Symbols -----------------------------------------------------

-- |
-- A list of 'Symbol's
type Symbols t = [Symbol t]
-- Token -------------------------------------------------------

-- |
-- A datatype for representing tokens with some builtins
-- and an aribitrary Token constructor.
-- This datatype stores (optional) lexemes.
data Token = Char       Char
           | Keyword    String
           | EOS
           | Epsilon
           | IntLit     (Maybe Int)
           | FloatLit   (Maybe Double)
           | BoolLit    (Maybe Bool)
           | StringLit  (Maybe String)
           | CharLit    (Maybe Char)
           | IDLit      (Maybe String)
           -- | alternative identifiers, for example functions vs. constructors (as in Haskell).
           | AltIDLit   (Maybe String)
           | Token String (Maybe String)
-- Tokens ------------------------------------------------------
-- |
-- A list of 'Token's
type Tokens = [Token]

-- | Class that captures elements of an input string (tokens).
--
-- * 'eos' is the end-of-string symbol
-- * 'eps' is the empty-string symbol
--
-- Both 'eos' and 'eps' must be distinct from eachother and from all
-- tokens in the input string.
-- The show instance is required to throw error messages.
class (Ord a, Eq a, Show a) => Parseable a where
    eos :: a
    eps :: a

    -- | This function is used for matching grammar tokens and input tokens.
    -- Override this method if, for example, your input tokens store lexemes
    -- while the grammar tokens do not
    matches :: a -> a -> Bool

    -- | This function pretty-prints the Parseable type by displaying its lexeme.
    -- Default implementation is 'show', which should be replaced for prettier error messages.
    unlex :: a -> String
    unlex = show

-- | Class whose members are super-types of 'Token'.
class SubsumesToken a where
    upcast :: Token -> a
    downcast :: a -> Maybe Token

instance SubsumesToken Token where
    upcast = id
    downcast = Just

deriving instance Ord Token
deriving instance Eq Token

instance Show Token where
    show (Char c)             = "keychar('" ++ [c] ++ "')"
    show (Keyword s)          = "keyword(\"" ++ s ++ "\")"
    show (EOS)                = "<end-of-string>"
    show (Epsilon)            = "<epsilon>"
    show (IntLit (Just i))    = "int(" ++  show i ++ ")"
    show (IntLit _)           = "<int>"
    show (FloatLit (Just i))  = "float(" ++  show i ++ ")"
    show (FloatLit _)         = "<float>"
    show (BoolLit (Just b))   = "bool(" ++  show b ++ ")"
    show (BoolLit _)          = "<bool>"
    show (StringLit (Just s)) = "string(\"" ++ s ++ "\")"
    show (StringLit _)        = "<string>"
    show (CharLit (Just c))   = "char('" ++ [c] ++ "')"
    show (CharLit Nothing)    = "<char>"
    show (AltIDLit (Just id)) = "altid(\"" ++ id ++ "\")"
    show (AltIDLit Nothing)   = "<altid>"
    show (IDLit  (Just id))   = "id(\"" ++ id ++ "\")"
    show (IDLit Nothing)      = "<id>"
    show (Token nm (Just s))  = nm ++ "(\"" ++ s ++ "\")"
    show (Token nm _)         = "<" ++ nm ++ ">"

instance Parseable Token where
    eos = EOS
    eps = Epsilon

    unlex = unlexToken

    Token k _   `matches` Token k' _   = k' == k
    Char c      `matches` Char c'      = c' == c
    Keyword k   `matches` Keyword k'   = k' == k
    EOS         `matches` EOS          = True
    Epsilon     `matches` Epsilon      = True
    StringLit _ `matches` StringLit _  = True
    CharLit _   `matches` CharLit _    = True
    IntLit _    `matches` IntLit _     = True
    FloatLit _  `matches` FloatLit _   = True
    BoolLit _   `matches` BoolLit _    = True
    AltIDLit _  `matches` AltIDLit _   = True
    IDLit _     `matches` IDLit _      = True
    _           `matches` _            = False


-- | Pretty-prints a list of 'Token's as a concatenation of their lexemes.
unlexTokens :: [Token] -> String
unlexTokens = Prelude.concatMap unlexToken

unlexToken :: Token -> String
unlexToken t = case t of
          Char c              -> [c]
          Keyword s           -> s
          IntLit (Just i)     -> show i
          BoolLit (Just b)    -> show b
          StringLit (Just s)  -> s
          CharLit (Just c)    -> [c]
          AltIDLit (Just s)   -> s
          IDLit (Just s)      -> s
          Token _ (Just s)    -> s
          _                   -> ""

-- some helpers

isNt (Nt _) = True
isNt _      = False

isTerm (Term _) = True
isTerm _        = False

instance (Show t) => Show (Slot t) where
    show (Slot x alpha beta) = show x ++ " ::= " ++ showRhs alpha ++ "." ++ showRhs beta
     where  showRhs [] = ""
            showRhs ((Term t):rhs) = show t ++ showRhs rhs
            showRhs ((Nt x):rhs)   = show x ++ showRhs rhs

instance (Show t) => Show (Symbol t) where
    show (Nt s)         = unpack s
    show (Term t)       = show t

deriving instance (Ord t) => Ord (Slot t)
deriving instance (Eq t) => Eq (Slot t)
deriving instance (Show t) => Show (Prod t)
deriving instance (Ord t) => Ord (Prod t)
deriving instance (Eq t) => Eq (Prod t)
deriving instance (Eq t) => Eq (Symbol t)
deriving instance (Ord t) => Ord (Symbol t)
