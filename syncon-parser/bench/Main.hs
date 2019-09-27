{-# LANGUAGE ViewPatterns #-}

module Main where

import Pre
import Result (Result(..))

import System.FilePath (takeFileName, isExtensionOf, (</>))
import System.Directory (listDirectory, doesDirectoryExist, doesFileExist)

import qualified Data.HashMap.Strict as M
import Data.List (partition)

import P1Lexing.Types (Token)
import qualified P1Lexing.Lexer as Lexer
import P2LanguageDefinition.Types (TypeName(..))
import qualified P2LanguageDefinition.Parser as LD
import qualified P2LanguageDefinition.BasicChecker as LD
import qualified P4Parsing.Parser as Parser
import P4Parsing.Types (SingleLanguage(..))

import Criterion.Main

main :: IO ()
main = do
  langFiles <- getLanguages "languages"
  defaultMain
    [ bgroup "parser" $ langToBench <$> langFiles ]

langToBench :: (FilePath, ([FilePath], ([FilePath], [FilePath]))) -> Benchmark
langToBench (lang, (defFiles, uncurry mappend -> srcFiles)) = env (mkEnv lang defFiles) $ \env ->
  bgroup lang $ benchSrc env <$> srcFiles
  where
    benchSrc ~(lexFile, precomputed) srcFile = env (doLexFile lexFile srcFile) $ \toks ->
      bgroup (takeFileName srcFile)
      [ bench "earley-forest" $ whnf (Parser.parseTokens precomputed) toks ]
    doLexFile lexFile path = lexFile path >>= \case
      Error _ -> die $ "Could not lex file " <> toS path
      Data toks -> return toks

getLanguages :: FilePath -> IO [(FilePath, ([FilePath], ([FilePath], [FilePath])))]
getLanguages languageDirectory = do
  languages <- listDir languageDirectory >>= filterM doesDirectoryExist
  forM languages $ \langDir -> do
    files <- listDir langDir >>= filterM doesFileExist
    let (defs, sources) = partition ("syncon" `isExtensionOf`) files
        (fails, successes) = partition ("fail_" `isPrefixOf`) sources
    return (takeFileName langDir, (defs, (successes, fails)))
  where
    listDir path = listDirectory path <&> fmap (path </>)

mkEnv lang defFiles = mkDf >>= \df -> (,) <$> mkLexer df <*> mkEarleyParser df
  where
    mkDf = do
      foldMapM LD.parseFile defFiles >>= \case
        Error _ -> die $ "Could not parse a syncon-file for language " <> toS lang
        Data tops -> case LD.mkDefinitionFile tops of
          Error _ -> die $ "Could not create a complete definition for language " <> toS lang
          Data df -> return df
    mkLexer df = case Parser.dfToLanguageTokens df & Lexer.allOneLanguage SingleLanguage of
      Error _ -> die $ "Could not generate a parser for lanugage " <> toS lang
      Data lexFile -> return lexFile
    mkEarleyParser df = case Parser.precomputeSingleLanguage @(Token SingleLanguage TypeName) df of
      Error _ -> die $ "Could not generate earley parser for language " <> toS lang
      Data precomputed -> return precomputed
