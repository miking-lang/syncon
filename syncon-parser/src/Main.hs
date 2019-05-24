{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import Pre
import Result (Result(..))

import FileAnnotation (annotate, putInTextTemplate)
import ErrorMessage (FormatError, formatErrors, formatError)

import Data.Data (Data)
import qualified Data.HashSet as S
import qualified Data.HashMap.Lazy as M
import qualified Data.Sequence as Seq
import Data.FileEmbed (embedFile)

import Data.Generics.Uniplate.Data (universe, universeBi)
import Data.Functor.Foldable (project, cata)
import Text.Show.Pretty (pPrint)

import P1Lexing.Types (Range(..), range, textualRange)
import qualified P1Lexing.Types as Lexer
import qualified P1Lexing.Lexer as Lexer

import qualified P2LanguageDefinition.Types as LD
import qualified P2LanguageDefinition.Parser as LD
import qualified P2LanguageDefinition.BasicChecker as LD
import qualified P2LanguageDefinition.Elaborator as LD

import qualified P4Parsing.Types as Parser
import qualified P4Parsing.Parser as Parser
import qualified P4Parsing.AmbiguityReporter as Parser

import qualified Data.Automaton.NVA as NVA
import qualified Data.Automaton.GraphViz as GraphViz

synconTokens :: Lexer.LanguageTokens Text
synconTokens = Lexer.LanguageTokens
  -- Literal tokens
  [ "token", "=", "syncon", ":", "{", ";", "}", "prefix", "postfix", "infix"
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
  pPrint $ fmap Lexer.textualToken <$> res

parseTest :: IO ()
parseTest = do
  res <- LD.parseFile "examples/bootstrap.syncon"
  pPrint res

-- checkFailTest :: IO ()
-- checkFailTest = do
--   tops <- LD.parseFile "examples/broken.syncon" >>= dataOrError'
--   df <- LD.mkDefinitionFile tops & dataOrError'
--   pPrint df

-- checkSuccessTest :: IO ()
-- checkSuccessTest = do
--   tops <- LD.parseFile "examples/bootstrap.syncon" >>= dataOrError'
--   df <- LD.mkDefinitionFile tops & dataOrError'
--   pPrint df

elaborationTest :: IO ()
elaborationTest = do
  tops <- LD.parseFile "examples/bootstrap.syncon" >>= dataOrError'
  df <- LD.mkDefinitionFile tops & dataOrError'
  pPrint $ LD.elaborate (LD.syncons df) (LD.forbids df) (LD.precedences df)

parse4Test :: IO ()
parse4Test = do
  tops <- LD.parseFile "examples/bootstrap.syncon" >>= dataOrError'
  df <- LD.mkDefinitionFile tops & dataOrError'
  parseFile <- Parser.parseSingleLanguage df & dataOrError'
  setOfNodes <- parseFile "examples/bootstrap.syncon" >>= dataOrError'
  pPrint setOfNodes

-- ambigReportingTest :: IO ()
-- ambigReportingTest = do
--   tops <- LD.parseFile "examples/ambig.syncon" >>= dataOrError'
--   df <- LD.mkDefinitionFile tops & dataOrError'
--   parseFile <- Parser.parseSingleLanguage df & dataOrError'
--   setOfNodes <- parseFile "examples/ambig.test" >>= dataOrError'
--   case Parser.report (LD.syncons df) setOfNodes of
--     Data nodes -> pPrint nodes
--     Error errs -> pPrint $ errs <&> \case
--       -- Parser.Ambiguity r alts -> (r, fmap (project >>> fmap (project >>> void)) $ toList alts)
--       Parser.Ambiguity r alts -> (r, fmap (project >>> void) $ toList alts)

parseToHTMLDebug :: [FilePath] -> FilePath -> FilePath -> IO ()
parseToHTMLDebug defFiles sourceFile outFile = do
  defSource :: HashMap Text Text <- defFiles <&> toS & S.fromList & S.toMap & M.traverseWithKey (\path _ -> readFile $ toS path)
  putStrLn @Text "Parsing definition file"
  tops <- M.traverseWithKey (\defFile _ -> LD.parseFile $ toS defFile) defSource
          >>= (fold >>> dataOrError defSource)
  df <- LD.mkDefinitionFile tops & dataOrError defSource
  parseFile <- Parser.parseSingleLanguage df & dataOrError defSource
  fileSource <- readFile sourceFile <&> M.singleton (toS sourceFile)
  putStrLn @Text "Parsing source file"
  setOfNodes <- parseFile sourceFile >>= dataOrError fileSource
  case Parser.report df setOfNodes of
    Data node -> universe node >>= nodeAnnotation
      & annotate fileSource
      & putInTextTemplate (toS $(embedFile "resources/htmlTemplate.html"))
      & writeFile outFile
      & (>> putStrLn @Text "Done")
    Error errs -> formatError <$> errs
      & formatErrors fileSource
      & putStrLn

nodeAnnotation :: Parser.Node l LD.TypeName -> [(Range, Text)]
nodeAnnotation n = (range n, Parser.n_name n & coerce)
  : cata findToks (Parser.Struct $ Parser.n_contents n)
  where
    findToks (Parser.TokenLeafF tok) = [tokToAnno tok]
    findToks f = fold f
    tokToAnno (Lexer.LitTok r _ t) = (r, "literal " <> show t)
    tokToAnno (Lexer.OtherTok r _ (LD.TypeName tyn) t) = (r, "token " <> show tyn <> " " <> show t)

dataOrError :: (Functor f, Foldable f, FormatError e) => HashMap Text Text -> Result (f e) a -> IO a
dataOrError _ (Data a) = return a
dataOrError source (Error e) = do
  putStrLn $ formatErrors source $ formatError <$> e
  compErr "Main.dataOrError" "Got error"

dataOrError' :: Show e => Result e a -> IO a
dataOrError' (Data a) = return a
dataOrError' (Error e) = do
  pPrint e
  compErr "Main.dataOrError" "Got error"

testReduce :: IO ()
testReduce = do
  GraphViz.writeDotFile "out/pre.dot" show NVA.ppFakeEdge (NVA.asNFA nva)
  GraphViz.writeDotFile "out/post.dot" show NVA.ppFakeEdge (NVA.asNFA post)
  where
    nva :: NVA.NVA Int Text Text Text Text
    nva = NVA.NVA
      { NVA.initial = S.singleton 1
      , NVA.final = S.singleton 1
      , NVA.openTransitions = NVA.fromTriples
        [ (1, "c", ("gamma1", 2))
        , (2, "c1", ("gamma2", 2))
        , (2, "c2", ("gamma3", 2)) ]
      , NVA.closeTransitions = NVA.fromTriples
        [ (2, "r", ("gamma3", 3))
        , (3, "r", ("gamma2", 2))
        , (2, "r", ("gamma1", 1)) ]
      , NVA.innerTransitions = M.empty }
    post = NVA.reduce nva

test :: IO ()
test = parseToHTMLDebug ["case-studies/ocaml.syncon"] "case-studies/fizzbuzz.ml" "out.html"

getArgsSeq :: IO (Seq [Char])
getArgsSeq = getArgs <&> Seq.fromList

main :: IO ()
main = getArgsSeq >>= \case
  defFiles Seq.:|> sourceFile Seq.:|> outFile -> parseToHTMLDebug (toList defFiles) sourceFile outFile
  args -> putStrLn $
    "Expected arguments: <syncon-def-file(s)> <source-file> <output-html-file>\n"
    <> "Got " <> show (length args) <> " arguments: " <> (intercalate " " $ show <$> toList args)
