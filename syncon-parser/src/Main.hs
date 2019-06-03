{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import Pre
import Result (Result(..))

import FileAnnotation (annotate, putInTextTemplate)
import ErrorMessage (FormatError, formatErrors, formatError)

import System.Environment (withArgs)
import System.FilePath ((</>))

import Data.Data (Data)
import Data.List (partition)
import qualified Data.HashSet as S
import qualified Data.HashMap.Lazy as M
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Data.FileEmbed (embedFile)

import Data.Generics.Uniplate.Data (universe, universeBi)
import Data.Functor.Foldable (project, cata)
import Text.Show.Pretty (pPrint)

import qualified Options.Applicative as Opt

import P1Lexing.Types (Range(..), range, textualRange)
import qualified P1Lexing.Types as Lexer
import qualified P1Lexing.Lexer as Lexer

import qualified P2LanguageDefinition.Types as LD
import qualified P2LanguageDefinition.Parser as LD
import qualified P2LanguageDefinition.BasicChecker as LD
import qualified P2LanguageDefinition.Elaborator as LD

import qualified P4Parsing.Types as Parser
import qualified P4Parsing.Parser as Parser

import qualified P5DynamicAmbiguity.Types as DynAmb
import qualified P5DynamicAmbiguity.AmbiguityReporter as DynAmb
import qualified P5DynamicAmbiguity.TreeLanguage as DynAmb

import qualified Data.Automaton.NVA as NVA
import qualified Data.Automaton.GraphViz as GraphViz

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
dataOrError source (Error e) =
  die $ formatErrors source $ formatError <$> e

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
test = withArgs ["case-studies/ocaml.syncon", "case-studies/ocaml/inside_out.ml", "--html=out.html"] main
-- test = withArgs ["--help"] main

getArgsSeq :: IO (Seq [Char])
getArgsSeq = getArgs <&> Seq.fromList

common :: Opt.Parser (IO ())
common = do
  html <- optional $ Opt.strOption
    $ Opt.long "html"
    <> Opt.metavar "FILE"
    <> Opt.help "Output the result of parsing as a debug HTML file."
  outdir <- optional $ Opt.strOption
    $ Opt.long "out"
    <> Opt.metavar "DIR"
    <> Opt.help "\"Pretty\" print all src files to paths relative to DIR. Only works if all files where given as relative paths. Use '.' to overwrite the originals."
  extraSrcFiles <- many $ Opt.strOption
    $ Opt.long "source"
    <> Opt.metavar "FILE"
    <> Opt.help "Parse this file as a source file, even if its extension is syncon. Can be supplied multiple times."
  files <- some $ Opt.argument Opt.str $
    Opt.metavar "FILES..."

  pure $ do
    let (defFiles, srcFiles) = partition (".syncon" `Text.isSuffixOf`) files

    defSources <- defFiles & S.fromList & S.toMap
      & M.traverseWithKey (\path _ -> readFile $ toS path)
    putStrLn @Text "Parsing definition files(s)"
    tops <- M.traverseWithKey (\defFile _ -> LD.parseFile $ toS defFile) defSources
      >>= (fold >>> dataOrError defSources)
    df <- LD.mkDefinitionFile tops & dataOrError defSources
    parseFile <- Parser.parseSingleLanguage df & dataOrError defSources
    let pl = DynAmb.precompute df

    srcSources <- extraSrcFiles <> srcFiles & S.fromList & S.toMap
      & M.traverseWithKey (\path _ -> readFile $ toS path)
    srcNodes <- flip M.traverseWithKey srcSources $ \path _ -> do
      putStrLn @Text $ "Parsing \"" <> path <> "\""
      setOfNodes <- parseFile (toS path) >>= dataOrError srcSources
      case DynAmb.report pl setOfNodes of
        Data node -> return node
        Error errs -> formatError <$> errs
          & formatErrors srcSources
          & die

    forM_ html $ \htmlPath -> do
      putStrLn @Text $ "Writing HTML to \"" <> toS htmlPath <> "\""
      toList srcNodes >>= universe >>= nodeAnnotation
        & annotate srcSources
        & putInTextTemplate (toS $(embedFile "resources/htmlTemplate.html"))
        & writeFile htmlPath

    forM_ outdir $ \outPath ->
      forM_ (M.toList srcNodes) $ \(path, node) -> do
        let fullPath = outPath </> toS path
        putStrLn @Text $ "Writing to \"" <> toS fullPath <> "\""
        DynAmb.fastShortest pl node
          <&> DynAmb.textualToken
          & Seq.intersperse " "
          & fold
          & writeFile fullPath

    putStrLn @Text "All done"

main :: IO ()
main = join $ Opt.execParser $ Opt.info (common <**> Opt.helper)
  $ Opt.fullDesc
  <> Opt.progDesc "Parse files using syncons. Files with the '.syncon' extension will be treated as definition files and used to generate a parser, which will then be run on the remaining files."
  <> Opt.header "syncon-parser -- A proof-of-concept parser based on syncons"
