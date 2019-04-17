{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import Pre
import Result (Result(..))

import Text.Show.Pretty (pPrint)

import P1Lexing.Types (Range(..))
import qualified P1Lexing.Types as Lexer
import qualified P1Lexing.Lexer as Lexer

import qualified P2LanguageDefinition.Types as LD
import qualified P2LanguageDefinition.Parser as LD
import qualified P2LanguageDefinition.BasicChecker as LD
import qualified P2LanguageDefinition.Elaborator as LD

synconTokens :: Lexer.LanguageTokens Text
synconTokens = Lexer.LanguageTokens
  -- Literal tokens
  [ "token", "=", "syncon", ":", "{", ";", "}", "prefix", "postfix", "infix", "#assoc"
  , "(", ")", "*", "+", "?", ".", "comment", "left", "right", "precedence", "except"
  , "type", "builtin", "forbid" ]
  -- Regex tokens
  [ ("Name", (Nowhere, "[[:lower:]][[:word:]]*"))
  , ("TypeName", (Nowhere, "[[:upper:]][[:word:]]*"))
  , ("String", (Nowhere, "\"(\\\\.|[^\"\\\\])*\"")) ]
  -- Comment regex
  [(Nowhere, "//[^\\n]*(\\n|$)")]

lexTest :: IO ()
lexTest = do
  res <- Lexer.allOneLanguage @Text "SynconDef" synconTokens "examples/bootstrap.syncon"
  pPrint $ fmap Lexer.tokenText <$> res

parseTest :: IO ()
parseTest = do
  res <- LD.parseFile "examples/bootstrap.syncon"
  pPrint res

checkFailTest :: IO ()
checkFailTest = do
  res <- LD.parseFile "examples/broken.syncon"
  case res of
    Data tops -> pPrint $ LD.mkDefinitionFile tops
    Error _ -> pPrint res

checkSuccessTest :: IO ()
checkSuccessTest = do
  res <- LD.parseFile "examples/bootstrap.syncon"
  case res of
    Data tops -> pPrint $ LD.mkDefinitionFile tops
    Error _ -> pPrint res

elaborationTest :: IO ()
elaborationTest = do
  res <- LD.parseFile "examples/bootstrap.syncon"
  case res of
    Error _ -> pPrint res
    Data tops -> case LD.mkDefinitionFile tops of
      res'@Error{} -> pPrint res'
      Data defFile -> pPrint $
        LD.elaborate (LD.syncons defFile) (LD.forbids defFile) (LD.precedences defFile)

main :: IO ()
main = elaborationTest
