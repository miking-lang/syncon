module Main where

import Pre
import Result (Result(..))

import System.FilePath (takeFileName, isExtensionOf, (</>))
import System.Directory (listDirectory, doesDirectoryExist, doesFileExist)

import qualified Data.HashMap.Strict as M
import Data.List (partition)

import qualified P1Lexing.Lexer as Lexer
import qualified P2LanguageDefinition.Parser as LD
import qualified P2LanguageDefinition.BasicChecker as LD
import qualified P4Parsing.Parser as Parser
import P4Parsing.Types (SingleLanguage(..))

import Criterion.Main

languageDirectory :: FilePath
languageDirectory = "bench/languages"

listDir :: FilePath -> IO [FilePath]
listDir path = listDirectory path <&> fmap (path </>)

main :: IO ()
main = do
  languages <- listDir languageDirectory >>= filterM doesDirectoryExist
  langFiles <- forM languages $ \langDir -> do
    files <- listDir langDir >>= filterM doesFileExist
    return (takeFileName langDir, partition ("syncon" `isExtensionOf`) files)
  defaultMain
    [ bgroup "parser" $ langToBench <$> langFiles ]

langToBench :: (FilePath, ([FilePath], [FilePath])) -> Benchmark
langToBench (lang, (defFiles, srcFiles)) = env mkParser $ \ ~(lexFile, parseTokens) ->
  bgroup lang $ benchSrc lexFile parseTokens <$> srcFiles
  where
    mkParser = do
      foldMapM LD.parseFile defFiles >>= \case
        Error _ -> die $ "Could not parse a syncon-file for language " <> toS lang
        Data tops -> case LD.mkDefinitionFile tops of
          Error _ -> die $ "Could not create a complete definition for language " <> toS lang
          Data df -> case Parser.parseTokens @SingleLanguage @[] df of
            Error _ -> die $ "Could not generate a parser for language " <> toS lang
            Data parseTokens -> do
              evaluate $ parseTokens []  -- try to materialize the shared datastructures, so the first run isn't a lot slower than the rest
              case Parser.dfToLanguageTokens df & Lexer.allOneLanguage SingleLanguage of
                Error _ -> die $ "Could not generate lexer for language " <> toS lang
                Data lexFile -> return (lexFile, parseTokens)
    benchSrc lexFile parseTokens srcFile = env (doLexFile lexFile srcFile) $ \toks ->
      bench (takeFileName srcFile) $ whnf parseTokens toks
    doLexFile lexFile path = lexFile path >>= \case
      Error _ -> die $ "Could not lex file " <> toS path
      Data toks -> return toks
