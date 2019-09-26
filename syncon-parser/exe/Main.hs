{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-deprecations #-}

module Main where

import Pre hiding ((<.>))
import Result (Result(..))

import FileAnnotation (annotate, putInTextTemplate)
import ErrorMessage (FormatError, formatErrors, formatError, ErrorOpts)

import System.Environment (withArgs)
import System.FilePath ((</>), (<.>), takeDirectory)
import System.Timeout (timeout)
import System.Directory (createDirectoryIfMissing)

import Data.Data (Data)
import Data.IORef (newIORef, modifyIORef', readIORef)
import Data.List (partition)
import qualified Data.HashSet as S
import qualified Data.HashMap.Lazy as M
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as LByteString
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
import qualified P4Parsing.Parser2 as Parser2

import qualified P5DynamicAmbiguity.Types as DynAmb
import qualified P5DynamicAmbiguity.TreeLanguage as DynAmb
import qualified P5DynamicAmbiguity.Isolation as DynAmb
import qualified P5DynamicAmbiguity.Analysis as DynAmb

import qualified P6Output.JsonV1 as Output

import qualified Data.Automaton.NVA as NVA
import qualified Data.Automaton.GraphViz as GraphViz

nodeAnnotation :: Parser.Node (Lexer.Token l LD.TypeName) -> [(Range, Text)]
nodeAnnotation n = (range n, Parser.n_name n & coerce)
  : cata findToks (Parser.Struct $ Parser.n_contents n)
  where
    findToks (Parser.TokenLeafF tok) = [tokToAnno tok]
    findToks f = fold f
    tokToAnno (Lexer.LitTok r _ t) = (r, "literal " <> show t)
    tokToAnno (Lexer.OtherTok r _ (LD.TypeName tyn) t) = (r, "token " <> show tyn <> " " <> show t)

dataOrError :: (Functor f, Foldable f, FormatError e) => HashMap Text Text -> ErrorOpts e -> Result (f e) a -> IO a
dataOrError _ _ (Data a) = return a
dataOrError source opts (Error e) =
  die $ formatErrors source $ formatError opts <$> e

dataOrError' :: (Functor f, Foldable f, FormatError e) => HashMap Text Text -> ErrorOpts e -> Result (f e) a -> IO a
dataOrError' _ _ (Data a) = return a
dataOrError' source opts (Error e) =
  die' $ formatErrors source $ formatError opts <$> e

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
-- test = withArgs ["languages/lispy/lispy.syncon", "languages/lispy/lispy.test", "--two-level"] main
-- test = withArgs ["languages/simple-right-recursion/def.syncon", "languages/simple-right-recursion/test.test", "--two-level"] main
-- test = withArgs ["languages/atest/atest.syncon", "languages/atest/atest.test", "--two-level"] main
-- test = withArgs ["languages/ocaml/ocaml.syncon", "languages/ocaml/a_ustring.ml", "--html=out.html", "--two-level"] main
test = withArgs ["languages/ocaml/ocaml.syncon", "languages/ocaml/ustring.ml", "--html=out.html", "--two-level"] main
-- test = withArgs ["case-studies/ocaml.syncon", "case-studies/ocaml/fizzbuzz.ml", "--html=out.html", "--two-level"] main
-- test = withArgs ["examples/ambig.syncon", "examples/ambig.test", "--dot=out"] main
-- test = withArgs ["examples/ambig.syncon", "examples/ambig.test", "--two-level"] main
-- test = withArgs ["examples/bootstrap.syncon", "--source=examples/bootstrap.syncon", "--json=out.json"] main
-- test = withArgs ["--help"] main
-- test = GLL.test

getArgsSeq :: IO (Seq [Char])
getArgsSeq = getArgs <&> Seq.fromList

newtype SourceFileException = SourceFileException Text deriving (Show)
instance Exception SourceFileException

die' :: Text -> IO a
die' t = do
  throwIO $ SourceFileException t

common :: Opt.Parser (IO ())
common = do
  html <- optional $ Opt.strOption
    $ Opt.long "html"
    <> Opt.metavar "FILE"
    <> Opt.help "Output the result of parsing as a debug HTML file."
  json <- optional $ Opt.strOption
    $ Opt.long "json"
    <> Opt.metavar "FILE"
    <> Opt.help "Output the ASTs as machine-readable JSON."
  -- outdir <- optional $ Opt.strOption
  --   $ Opt.long "out"
  --   <> Opt.metavar "DIR"
  --   <> Opt.help "\"Pretty\" print all src files to paths relative to DIR. Only works if all files were given as relative paths. Use '.' to overwrite the originals."
  dot <- optional $ Opt.strOption
    $ Opt.long "dot"
    <> Opt.metavar "DIR"
    <> Opt.help "Output the parse forests as graphviz dot files relative to DIR. Only works if all files were given as relative paths."
  extraSrcFiles <- many $ Opt.strOption
    $ Opt.long "source"
    <> Opt.metavar "FILE"
    <> Opt.help "Parse this file as a source file, even if its extension is '.syncon'. Can be supplied multiple times."
  showTwoLevel <- Opt.switch
    $ Opt.long "two-level"
    <> Opt.help "Always show the two level representation, even if some alternatives are resolvable."
  sourceTimeout <- fmap (*1_000_000) $ Opt.option Opt.auto
    $ Opt.long "timeout"
    <> Opt.metavar "S"
    <> Opt.help "Timeout for attempting to parse a single source file, in seconds. A negative value means 'wait forever'."
    <> Opt.value (-1)
  dynAmbTimeout <- fmap (*1_000) $ Opt.option Opt.auto
    $ Opt.long "dynamic-resolvability-timeout"
    <> Opt.metavar "MS"
    <> Opt.help "Timeout for determining if a single ambiguity is resolvable, in milliseconds. A negative value means 'wait forever'."
    <> Opt.value 1_000
  continueAfterError <- Opt.switch
    $ Opt.long "continue-after-error"
    <> Opt.help "Don't abort after the first source file that gives errors."
  files <- some $ Opt.argument Opt.str $
    Opt.metavar "FILES..."

  pure $ do
    let (defFiles, srcFiles) = partition (".syncon" `Text.isSuffixOf`) files

    defSources <- defFiles & S.fromList & S.toMap
      & M.traverseWithKey (\path _ -> readFile $ toS path)
    putStrLn @Text "Parsing definition files(s)"
    tops <- M.traverseWithKey (\defFile _ -> LD.parseFile $ toS defFile) defSources
      >>= (fold >>> dataOrError defSources ())
    df <- LD.mkDefinitionFile tops & dataOrError defSources ()
    preParse <- Parser2.precomputeSingleLanguage @(Lexer.Token Parser.SingleLanguage LD.TypeName) df & dataOrError defSources ()
    let pl = DynAmb.precompute df

    srcSources <- extraSrcFiles <> srcFiles & S.fromList & S.toMap
      & M.traverseWithKey (\path _ -> readFile $ toS path)
    successfulFiles <- newIORef @Int 0
    failureFiles <- newIORef @Int 0
    let sourceFailureHandler
          | continueAfterError = \t -> putStrLn t >> return undefined  -- NOTE: this undefined is ok, since this case will only happen after we have recorded a failure, which means that we stop processing immediately after finishing constructing this map, i.e., its values will never be used
          | otherwise = die
    srcNodes <- flip M.traverseWithKey srcSources $ \path _ -> do
      putStrLn @Text $ "Parsing \"" <> path <> "\""
      handle (\(SourceFileException t) -> modifyIORef' failureFiles (+1) >> sourceFailureHandler t) $ do
        mNode <- timeout sourceTimeout $ do
          forest@(nodeMap, _) <- Parser2.parseFile preParse (toS path) >>= dataOrError' srcSources ()
          forM_ dot $ \outPath -> do
            let fullPath = outPath </> toS path <.> "dot"
            putStrLn @Text $"Writing to \"" <> toS fullPath <> "\""
            createDirectoryIfMissing True $ takeDirectory fullPath
            Parser2.forestToDot (Parser.n_nameF >>> coerce) forest
              & writeFile fullPath
          DynAmb.isolate forest & \case
            Data node -> modifyIORef' successfulFiles (+1) >> return node
            Error ambs -> ambs
              & mapM (foldMap S.singleton >>> DynAmb.analyze dynAmbTimeout pl DynAmb.convertToken (DynAmb.getElidable pl nodeMap) (DynAmb.showElidable nodeMap))
              <&> fmap (formatError DynAmb.EO{DynAmb.showTwoLevel, DynAmb.showElided = DynAmb.showElidable nodeMap, DynAmb.elidedRange = DynAmb.getElidable pl nodeMap >>> fst})
              <&> formatErrors srcSources
              & (>>= die')
        maybe (die' "        timeout when parsing file") return mNode

    numSuccesses <- readIORef successfulFiles
    numFailures <- readIORef failureFiles
    putStrLn @Text $ "Parsed " <> show numSuccesses <> " files successfully, failed on " <> show numFailures <> " files."

    when (numFailures /= 0) exitFailure

    forM_ html $ \htmlPath -> do
      putStrLn @Text $ "Writing HTML to \"" <> toS htmlPath <> "\""
      toList srcNodes >>= universe >>= nodeAnnotation
        & annotate srcSources
        & putInTextTemplate (toS $(embedFile "resources/htmlTemplate.html"))
        & writeFile htmlPath

    forM_ json $ \jsonPath -> do
      putStrLn @Text $ "Writing JSON to \"" <> toS jsonPath <> "\""
      LByteString.writeFile jsonPath $ Output.encode srcNodes

    -- forM_ outdir $ \outPath ->
    --   forM_ (M.toList srcNodes) $ \(path, node) -> do
    --     let fullPath = outPath </> toS path
    --     putStrLn @Text $ "Writing to \"" <> toS fullPath <> "\""
    --     createDirectoryIfMissing True $ takeDirectory fullPath
    --     DynAmb.fastShortest pl node
    --       <&> DynAmb.textualToken
    --       & Seq.intersperse " "
    --       & fold
    --       & writeFile fullPath

    putStrLn @Text "All done"

main :: IO ()
main = join $ Opt.execParser $ Opt.info (common <**> Opt.helper)
  $ Opt.fullDesc
  <> Opt.progDesc "Parse files using syncons. Files with the '.syncon' extension will be treated as definition files and used to generate a parser, which will then be run on the remaining files."
  <> Opt.header "syncon-parser -- A proof-of-concept parser based on syncons"
