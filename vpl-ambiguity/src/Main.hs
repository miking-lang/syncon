module Main where

import Pre

import Data.String (fromString)

import qualified Data.HashMap.Lazy as M

import qualified Regex as R
import qualified Automaton.NFA as N

main :: IO ()
main = putStrLn @Text "blub"

-- Terminal, NonTerminal, LeftParen, RightParen
data Alphabet t nt = T t | NT nt | L | R

type Language = HashMap Text (R.Regex (Alphabet Text Text))

instance IsString t => IsString (Alphabet t nt) where
  fromString = fromString >>> T

-- returns (the automaton, a mapping from non-terminals to their final states)
toParenNFA :: Language -> (N.NFA Int (Alphabet Text Text), HashMap Text (HashSet Int))
toParenNFA = undefined

exprGrammar :: Language
exprGrammar = M.fromList
  [("E",
    R.choice
     [ R.concat [e, ";", e]
     , R.concat ["[", R.opt $ R.Concat e (R.star $ R.Concat ";" e), "]"]
     , "a"])]
  where
    e = R.Terminal $ NT "E"
