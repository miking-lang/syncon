{-# OPTIONS_GHC -Wno-missing-signatures #-}

module ParseTest (test) where

import Prelude ()
import Pre
import Result (Result(..))

import System.FilePath (takeFileName, isExtensionOf, (</>))
import System.Directory (listDirectory, doesDirectoryExist, doesFileExist)

import Data.IORef (newIORef, readIORef, IORef, writeIORef)
import Data.List (partition)

import P1Lexing.Types (Token)
import qualified P1Lexing.Lexer as Lexer
import P2LanguageDefinition.Types (TypeName(..), PrecedenceKind(PermissivePrecedence))
import qualified P2LanguageDefinition.Parser as LD
import qualified P2LanguageDefinition.BasicChecker as LD
import qualified P4Parsing.Parser as Parser
import P4Parsing.Types (SingleLanguage(..))

test :: IO Bool
test = do
  failRef <- newIORef False
  langFiles <- getLanguages "languages"
  mapM_ (langToTest failRef) langFiles
  not <$> readIORef failRef

langToTest :: IORef Bool -> (FilePath, ([FilePath], ([FilePath], [FilePath]))) -> IO ()
langToTest failRef (lang, (defFiles, (successFiles, failFiles))) = do
  env <- mkEnv lang defFiles
  mapM_ (mkTest env True) successFiles
  mapM_ (mkTest env False) failFiles
  where
    mkTest (lexFile, precomputed) expected srcFile = do
      toks <- doLexFile lexFile srcFile
      let earleyResult = Parser.parseTokens precomputed toks & isData & (== expected)
          showRes True  = "    "
          showRes False = "FAIL"
      putStrLn $ showRes earleyResult <> " parser/" <> lang <> "/" <> takeFileName srcFile <> "/earley-forest"
      when (not earleyResult) $ writeIORef failRef True
    doLexFile lexFile path = lexFile path >>= \case
      Error _ -> die $ "Could not lex file " <> toS path
      Data toks -> return toks
    isData (Data (a, b)) = a `deepseq` b `deepseq` True
    isData _ = False

getLanguages :: FilePath -> IO [(FilePath, ([FilePath], ([FilePath], [FilePath])))]
getLanguages languageDirectory = do
  languages <- listDir languageDirectory >>= filterM doesDirectoryExist
  forM languages $ \langDir -> do
    files <- listDir langDir >>= filterM doesFileExist
    let (defs, sources) = partition ("syncon" `isExtensionOf`) files
        (fails, successes) = partition (takeFileName >>> ("fail_" `isPrefixOf`)) sources
    return (takeFileName langDir, (defs, (successes, fails)))
  where
    listDir path = listDirectory path <&> fmap (path </>)

mkEnv lang defFiles = mkDf >>= \df -> (,) <$> mkLexer df <*> mkEarleyParser df
  where
    mkDf = do
      foldMapM LD.parseFile defFiles >>= \case
        Error _ -> die $ "Could not parse a syncon-file for language " <> toS lang
        Data tops -> case LD.mkDefinitionFile PermissivePrecedence tops of
          Error _ -> die $ "Could not create a complete definition for language " <> toS lang
          Data df -> return df
    mkLexer df = case Parser.dfToLanguageTokens df & Lexer.allOneLanguage SingleLanguage of
      Error _ -> die $ "Could not generate a parser for lanugage " <> toS lang
      Data lexFile -> return lexFile
    mkEarleyParser df = case Parser.precomputeSingleLanguage @(Token SingleLanguage TypeName) df of
      Error _ -> die $ "Could not generate earley parser for language " <> toS lang
      Data precomputed -> return precomputed
