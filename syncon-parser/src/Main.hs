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
import qualified P4Parsing.Parser as Parser

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
  res <- Lexer.allOneLanguage' @Text "SynconDef" synconTokens "examples/bootstrap.syncon"
  pPrint $ fmap Lexer.tokenText <$> res

parseTest :: IO ()
parseTest = do
  res <- LD.parseFile "examples/bootstrap.syncon"
  pPrint res

checkFailTest :: IO ()
checkFailTest = do
  tops <- LD.parseFile "examples/broken.syncon" >>= dataOrError
  df <- LD.mkDefinitionFile tops & dataOrError
  pPrint df

checkSuccessTest :: IO ()
checkSuccessTest = do
  tops <- LD.parseFile "examples/bootstrap.syncon" >>= dataOrError
  df <- LD.mkDefinitionFile tops & dataOrError
  pPrint df

elaborationTest :: IO ()
elaborationTest = do
  tops <- LD.parseFile "examples/bootstrap.syncon" >>= dataOrError
  df <- LD.mkDefinitionFile tops & dataOrError
  pPrint $ LD.elaborate (LD.syncons df) (LD.forbids df) (LD.precedences df)

parse4Test :: IO ()
parse4Test = do
  putStrLn @Text "Initial parse"
  tops <- LD.parseFile "examples/bootstrap.syncon" >>= dataOrError
  df <- LD.mkDefinitionFile tops & dataOrError
  putStrLn @Text "Second parse"
  parseFile <- Parser.parseSingleLanguage df & dataOrError
  setOfNodes <- parseFile "examples/bootstrap.syncon" >>= dataOrError
  pPrint setOfNodes

dataOrError :: Show e => Result e a -> IO a
dataOrError (Data a) = return a
dataOrError (Error e) = do
  pPrint e
  compErr "Main.dataOrError" "Got error"

main :: IO ()
main = parse4Test
