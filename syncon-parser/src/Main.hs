module Main where

import Pre

import Text.Show.Pretty (pPrint)

import qualified P1Lexing.Lexer as Lexer
import qualified P1Lexing.Types as Lexer

synconTokens :: Lexer.LanguageTokens Text
synconTokens = Lexer.LanguageTokens
  ["token", "=", "syncon", ":", "{", ";", "}", "prefix", "postfix", "infix", "#assoc"
  , "(", ")", "*", "+", "?", "." ]
  [ ("Name", "[[:lower:]][[:word:]]*")
  , ("TypeName", "[[:upper:]][[:word:]]*")
  , ("String", "\"(\\\\.|[^\"\\\\])*\"") ]
  "//[^\\n]*\\n?" -- Comment regex

main :: IO ()
main = do
  res <- Lexer.allOneLanguage @Text "SynconDef" synconTokens "examples/bootstrap.syncon"
  pPrint $ fmap Lexer.tokenText <$> res
