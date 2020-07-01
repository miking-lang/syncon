{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-deprecations #-}

module Main where

import Pre hiding ((<.>), race)
import Result (Result(..))
import Data.String (fromString)
import qualified Text.Printf as Printf

import FileAnnotation (annotate, putInTextTemplate)
import ErrorMessage (FormatError, formatErrors, formatError, ErrorOpts)

import System.Environment (withArgs)
import System.FilePath ((</>), (<.>), takeDirectory, isExtensionOf)
import System.Timeout (timeout)
import System.Directory (createDirectoryIfMissing)

import Control.Concurrent.Async.Lifted (race)

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
import Codec.Serialise (writeFileSerialise, readFileDeserialise, DeserialiseFailure(..))

import qualified Options.Applicative as Opt
import qualified Hedgehog
import qualified Text.Earley.Forest.Grammar as Forest

import P1Lexing.Types (Range(..), range, textualRange)
import qualified P1Lexing.Types as Lexer
import qualified P1Lexing.Lexer as Lexer

import qualified P2LanguageDefinition.Types as LD
import qualified P2LanguageDefinition.Parser as LD
import qualified P2LanguageDefinition.BasicChecker as LD
import qualified P2LanguageDefinition.Elaborator as LD

import qualified P4Parsing.Types as Parser
import qualified P4Parsing.Parser as Parser
import qualified P4Parsing.Generator as Parser

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
-- test = withArgs ["languages/ocaml/ocaml.syncon", "languages/ocaml/ustring.ml", "--html=out.html", "--two-level"] main
-- test = withArgs ["case-studies/ocaml.syncon", "case-studies/ocaml/fizzbuzz.ml", "--html=out.html", "--two-level"] main
-- test = withArgs ["examples/ambig.syncon", "examples/ambig.test", "--dot=out"] main
-- test = withArgs ["examples/ambig.syncon", "examples/ambig.test", "--two-level"] main
-- test = withArgs ["examples/bootstrap.syncon", "--source=examples/bootstrap.syncon", "--json=out.json"] main
-- test = withArgs ["--help"] main
-- test = withArgs ["parse", "README.md", "source.test"] main
test = withArgs ["pbt", "examples/ambig.syncon"] main
-- test = GLL.test

getArgsSeq :: IO (Seq [Char])
getArgsSeq = getArgs <&> Seq.fromList

newtype SourceFileException = SourceFileException Text deriving (Show)
instance Exception SourceFileException

die' :: Text -> IO a
die' t = do
  throwIO $ SourceFileException t

compileAction :: LD.PrecedenceKind -> [FilePath] -> IO (LD.DefinitionFile, Parser.Precomputed Parser.SL, DynAmb.PreLanguage DynAmb.Elidable)
compileAction precKind files = do
  sources <- files <&> toS & S.fromList & S.toMap
    & M.traverseWithKey (\path _ -> readFile $ toS path)
  putStrLn @Text "Parsing definition file(s)"
  tops <- M.traverseWithKey (\defFile _ -> LD.parseFile $ toS defFile) sources
    >>= (fold >>> dataOrError sources ())
  df <- LD.mkDefinitionFile precKind tops & dataOrError sources ()
  preParse <- Parser.precomputeSingleLanguage @(Lexer.Token Parser.SingleLanguage LD.TypeName) df & dataOrError sources ()
  let pl = DynAmb.precompute @DynAmb.Elidable df
  return (df, preParse, pl)

parseAction :: Opt.Parser ((Parser.Precomputed Parser.SL, DynAmb.PreLanguage DynAmb.Elidable) -> [FilePath] -> IO ())
parseAction = do
  html <- optional $ Opt.strOption
    $ Opt.long "html"
    <> Opt.metavar "FILE"
    <> Opt.help "Output the result of parsing as a debug HTML file."
  json <- optional $ Opt.strOption
    $ Opt.long "json"
    <> Opt.metavar "FILE"
    <> Opt.help "Output the ASTs as machine-readable JSON."
  dot <- optional $ Opt.strOption
    $ Opt.long "dot"
    <> Opt.metavar "DIR"
    <> Opt.help "Output the parse forests as graphviz dot files relative to DIR. Only works if all files were given as relative paths."
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
  noIsolation <- Opt.switch
    $ Opt.long "no-isolation"
    <> Opt.help "Do not isolate ambiguities, instead treat the entire file as ambiguous."
  continueAfterError <- Opt.switch
    $ Opt.long "continue-after-error"
    <> Opt.help "Don't abort after the first source file that gives errors."

  pure $ \(preParse, pl) files -> do
    sources <- files <&> toS & S.fromList & S.toMap
      & M.traverseWithKey (\path _ -> readFile $ toS path)
    successfulFiles <- newIORef @Int 0
    failureFiles <- newIORef @Int 0
    let sourceFailureHandler
          | continueAfterError = \t -> putStrLn t >> return undefined  -- NOTE: this undefined is ok, since this case will only happen after we have recorded a failure, which means that we stop processing immediately after finishing constructing this map, i.e., its values will never be used
          | otherwise = die
        isolate = if noIsolation
                  then DynAmb.dummyIsolate >>> first Seq.singleton
                  else DynAmb.isolate
    srcNodes <- flip M.traverseWithKey sources $ \path _ -> do
      putStrLn $ "Parsing \"" <> path <> "\""
      handle (\(SourceFileException t) -> modifyIORef' failureFiles (+1) >> sourceFailureHandler t) $ do
        mNode <- timeout sourceTimeout $ do
          forest@(nodeMap, _) <- Parser.parseFile preParse (toS path) >>= dataOrError' sources ()
          forM_ dot $ \outPath -> do
            let fullPath = outPath </> toS path <.> "dot"
            putStrLn @Text $"Writing to \"" <> toS fullPath <> "\""
            createDirectoryIfMissing True $ takeDirectory fullPath
            Parser.forestToDot (Parser.n_nameF >>> coerce) forest
              & writeFile fullPath
          isolate forest & \case
            Data node -> modifyIORef' successfulFiles (+1) >> return node
            Error ambs ->
              let opts = DynAmb.EO
                    { DynAmb.showTwoLevel
                    , DynAmb.showElided = DynAmb.showElidable nodeMap
                    , DynAmb.elidedRange = DynAmb.getElidable pl nodeMap >>> fst
                    , DynAmb.showTok = Forest.unlex
                    , DynAmb.tokRange = range
                    }
              in ambs
                 & mapM (foldMap S.singleton >>> DynAmb.analyze dynAmbTimeout pl DynAmb.convertToken (DynAmb.getElidable pl nodeMap) (DynAmb.showElidable nodeMap))
                 <&> fmap (formatError opts)
                 <&> formatErrors sources
                 >>= die'
        maybe (die' "        timeout when parsing file") return mNode

    numSuccesses <- readIORef successfulFiles
    numFailures <- readIORef failureFiles
    putStrLn @Text $ "Parsed " <> show numSuccesses <> " files successfully, failed on " <> show numFailures <> " files."

    when (numFailures /= 0) exitFailure

    forM_ html $ \htmlPath -> do
      putStrLn @Text $ "Writing HTML to \"" <> toS htmlPath <> "\""
      toList srcNodes >>= universe >>= nodeAnnotation
        & annotate sources
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

    putStrLn @Text "Parsing done"

compileCommand :: Opt.Mod Opt.CommandFields (IO ())
compileCommand = Opt.command "compile" (Opt.info compileCmd $ Opt.progDesc "Compile a list of '.syncon' files to a single '.synconc' file, to be used for parsing.")
  where
    compileCmd = do
      precKind <- Opt.flag LD.PermissivePrecedence LD.RestrictivePrecedence
        $ Opt.long "restrictive-precedence"
        <> Opt.help "Restrict low-precedence unary operators to retain unambiguity in the presence of a total precedence list. This changes the recognized syntactic language, but not the semantic language."
      files <- some $ Opt.argument Opt.str
        $ Opt.metavar "FILES..."
      outputFile <- Opt.strOption
        $ Opt.long "output"
        <> Opt.short 'o'
        <> Opt.metavar "OUTPUT"

      pure $ do
        (_, preParse, pl) <- compileAction precKind files
        let serialisable = (Parser.precomputeToSerialisable preParse, pl)
        writeFileSerialise outputFile serialisable

parseCommand :: Opt.Mod Opt.CommandFields (IO ())
parseCommand = Opt.command "parse" (Opt.info parseCmd $ Opt.progDesc "Parse a list of files using a compiled '.synconc' file.")
  where
    parseCmd = do
      synconc <- Opt.argument Opt.str $ Opt.metavar "SYNCONC"
      parseAction' <- parseAction
      files <- some $ Opt.argument Opt.str $
        Opt.metavar "FILES..."

      pure $
        try (readFileDeserialise synconc) >>= \case
          Left DeserialiseFailure{} -> die $ "Could not parse '" <> toS synconc <> "' as a '.synconc' file, did you supply the right file first?"
          Right (preParseSerialisable, pl) -> do
            preParse <- Parser.serialisableToPrecompute @(Lexer.Token Parser.SingleLanguage LD.TypeName) preParseSerialisable & dataOrError mempty ()
            parseAction' (preParse, pl) files

devCommand :: Opt.Mod Opt.CommandFields (IO ())
devCommand = Opt.command "dev" (Opt.info devCmd $ Opt.progDesc "Compile and parse a language in one command, for use during language development.")
  where
    devCmd = do
      precKind <- Opt.flag LD.PermissivePrecedence LD.RestrictivePrecedence
        $ Opt.long "restrictive-precedence"
        <> Opt.help "Restrict low-precedence unary operators to retain unambiguity in the presence of a total precedence list. This changes the recognized syntactic language, but not the semantic language."
      parseAction' <- parseAction
      files <- some $ Opt.argument Opt.str $
        Opt.metavar "FILES..."
        <> Opt.help "'.syncon' files are treated as language definition files, all other files are parsed."

      pure $ do
        let (defFiles, srcFiles) = partition ("syncon" `isExtensionOf`) files
        (_, preParse, pl) <- compileAction precKind defFiles
        parseAction' (preParse, pl) srcFiles

data Ambiguity = UnresolvableAmbiguity | Ambiguity deriving (Show)
hasAmbStyle :: Ambiguity -> DynAmb.Error a b -> Bool
hasAmbStyle Ambiguity _ = True
hasAmbStyle UnresolvableAmbiguity err = case DynAmb.ambiguityStyle err of
  DynAmb.Unresolvable -> True
  DynAmb.Mixed -> True
  DynAmb.Resolvable -> False

pbtCommand :: Opt.Mod Opt.CommandFields (IO ())
pbtCommand = Opt.command "pbt" (Opt.info pbtCmd $ Opt.progDesc "Explore the ambiguity of a language using property based testing.")
  where
    pbtCmd = do
      precKind <- Opt.flag LD.PermissivePrecedence LD.RestrictivePrecedence
        $ Opt.long "restrictive-precedence"
        <> Opt.help "Restrict low-precedence unary operators to retain unambiguity in the presence of a total precedence list. This changes the recognized syntactic language, but not the semantic language."
      target <- Opt.flag UnresolvableAmbiguity Ambiguity
        $ Opt.long "ambiguity"
        <> Opt.help "Look for any kind of ambiguity, not just unresolvable ambiguity."
      numRuns <- Opt.option Opt.auto
        $ Opt.long "tests"
        <> Opt.metavar "N"
        <> Opt.value 10000
        <> Opt.help "The number of tests to run."
      numDiscards <- Opt.option Opt.auto
        $ Opt.long "discards"
        <> Opt.value 10000
        <> Opt.help "The maximum number of tests to discard. A test is discarded if the analysis times out."
      showSizeDistr <- Opt.switch
        $ Opt.long "size-distr"
        <> Opt.help "Show the distribution of CST sizes generated"
      showSynconDistr <- Opt.switch
        $ Opt.long "syncon-distr"
        <> Opt.help "Show the fraction of CSTs that contain each syncon"
      showSyTyDistr <- Opt.switch
        $ Opt.long "syty-distr"
        <> Opt.help "Show the fraction of CSTs that contain each syntax type"
      dynAmbTimeout <- fmap (*1_000) $ Opt.option Opt.auto
        $ Opt.long "timeout"
        <> Opt.metavar "MS"
        <> Opt.help "Timeout before discarding a test case, in milliseconds. A negative value means 'wait forever'."
        <> Opt.value 4_000
      showTwoLevel <- Opt.switch
        $ Opt.long "two-level"
        <> Opt.help "Always show the two level representation, even if some alternatives are resolvable."
      noIsolation <- Opt.switch
        $ Opt.long "no-isolation"
        <> Opt.help "Do not isolate ambiguities, instead treat the entire file as ambiguous."
      files <- some $ Opt.argument Opt.str $
        Opt.metavar "FILES..."
        <> Opt.help "The '.syncon' files that define the language to be tested."

      pure $ do
        (df, preParse, pl) <- compileAction precKind files
        let allSyncons = LD.syncons df & M.keysSet
            allSyTys = LD.syntaxTypes df & M.keysSet
            classifySyncon present n = Hedgehog.classify (coerce n & toS @Text & fromString) $ S.member n present
            classifySyTy present n = Hedgehog.classify (coerce n & toS @Text & fromString) $ S.member n present
            isolate = if noIsolation
                      then DynAmb.dummyIsolate >>> first Seq.singleton
                      else DynAmb.isolate
        let gen = Parser.programGenerator df
            prop = Hedgehog.withDiscards (fromInteger numDiscards) $ Hedgehog.withTests (fromInteger numRuns) $ Hedgehog.property $ do
              (size, syncons, types, Lexer.makeFakeFile -> (sources, program)) <- Hedgehog.forAllWith ((\(_, _, _, x) -> x) >>> toList >>> fmap Forest.unlex >>> Text.intercalate " " >>> toS) gen
              when showSizeDistr $
                Hedgehog.label $ fromString $ Printf.printf "%4d" size
              when showSynconDistr $
                mapM_ (classifySyncon syncons) allSyncons
              when showSyTyDistr $
                mapM_ (classifySyTy types) allSyTys
              discardSlow dynAmbTimeout $ do
                Hedgehog.evalM $ case Parser.parseTokens preParse program of
                  Error _ -> do
                    Hedgehog.annotate $ "Got error when parsing generated program, this should not be possible"
                    Hedgehog.failure
                  Data forest@(nodeMap, _) -> case isolate forest of
                    Data _ -> Hedgehog.success
                    Error ambs -> do
                      errs <- ambs
                        & mapM (foldMap S.singleton >>> DynAmb.analyze (-1) pl DynAmb.convertToken (DynAmb.getElidable pl nodeMap) (DynAmb.showElidable nodeMap))
                        <&> Seq.filter (hasAmbStyle target)
                        & lift
                      if Seq.null errs
                        then Hedgehog.success
                        else do
                        let opts = DynAmb.EO
                              { DynAmb.showTwoLevel
                              , DynAmb.showElided = DynAmb.showElidable nodeMap
                              , DynAmb.elidedRange = DynAmb.getElidable pl nodeMap >>> fst
                              , DynAmb.showTok = Forest.unlex
                              , DynAmb.tokRange = range
                              }
                        errs
                          <&> formatError opts
                          & formatErrors sources
                          & toS
                          & Hedgehog.annotate
                        Hedgehog.failure
        Hedgehog.check prop >>= \case
          True -> exitSuccess
          False -> exitFailure
    discardSlow :: Int -> Hedgehog.TestT IO a -> Hedgehog.PropertyT IO a
    discardSlow timelimit v = do
      result <- Hedgehog.test $ race (liftIO $ threadDelay timelimit) v
      case result of
        Left _ -> Hedgehog.discard
        Right a -> pure a

main :: IO ()
main = join $ Opt.execParser $ Opt.info (Opt.hsubparser (compileCommand <> parseCommand <> devCommand <> pbtCommand) <**> Opt.helper)
  $ Opt.fullDesc
  <> Opt.progDesc "Parse files using syncons. To parse a language defined with syncon you first have to compile it using the 'compile' command, then use the 'parse' command, giving it the file produced by 'compile'. Alternatively, you can use 'dev' to do both at once, though this is likely only desired during development, when the language definition changes frequently."
  <> Opt.header "syncon-parser -- A proof-of-concept parser based on syncons"
