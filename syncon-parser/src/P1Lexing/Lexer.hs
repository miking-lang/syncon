{-# LANGUAGE RecordWildCards #-}

module P1Lexing.Lexer
( LanguageTokens(..)
, Error(..)
, Lexer
, mkLexer
, startFileLex
, lexHere
, advanceLexer
, allOneLanguage
) where

import Pre

import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Array as Array
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as M

import Text.Regex.PCRE.ByteString (compile, execute, Regex, compUTF8, compNoAutoCapture)

import Result (Result(..))

import P1Lexing.Types

data LanguageTokens n = LanguageTokens
  [Text] -- ^ Literals
  [(n, Text)] -- ^ Other token kinds, with regexes in the texts
  Text -- ^ A regex for comments

data Error l n
  = RegexError l n Int Text
  | CommentRegexError l Int Text
  | UnicodeError UnicodeException
  | OverlappingLex l [Token l n]
  | NoLex Text
  deriving (Show)

data LanguageInternal n = LanguageInternal
  { literals :: !(Seq ByteString)
  , tokenRegexes :: !(Seq (n, Regex))
  , whitespaceRegex :: !Regex
  }

data Lexer l n = Lexer
  { remainingSource :: !ByteString
  , position :: !Position
  , languages :: !(HashMap l (LanguageInternal n))
  }

runRegex :: Regex -> ByteString -> Maybe ByteString
runRegex reg str = case unsafePerformIO (execute reg str) of
  Left err -> compErr "P1Lexing.Lexer.runRegex" $ show err
  Right Nothing -> Nothing
  Right (Just arr) -> case arr Array.! fst (Array.bounds arr) of
    (start, len) -> str & ByteString.drop start & ByteString.take len & Just

compileRegex :: Text -> Either (Int, Text) Regex
compileRegex t = unsafePerformIO (compile compOpts execOpts (encodeUtf8 ("^" <> t))) & first (second toS)
  where
    compOpts = compUTF8 .|. compNoAutoCapture
    execOpts = 0

-- | Construct a new lexer, capable of lexing multiple languages
mkLexer :: (Eq l, Hashable l) => [(l, LanguageTokens n)] -> Result [Error l n] (Lexer l n)
mkLexer langs = do
  langs' <- traverse mkLang langs
  return $ Lexer
    { remainingSource = ""
    , position = Position 1 1
    , languages = M.fromList langs' }
  where
    mkLang (l, LanguageTokens lits others comments) = do
      let lits' = Seq.fromList $ encodeUtf8 <$> lits
      others' <- Seq.fromList <$> traverse (mkRegex l) others
      whitespace <- mkWhitespace l comments
      return . (l,) $ LanguageInternal
        { literals = lits'
        , tokenRegexes = others'
        , whitespaceRegex = whitespace }
    mkRegex l (n, t) = case compileRegex t of
      Left (column, message) -> Error [RegexError l n column message]
      Right reg -> Data (n, reg)
    mkWhitespace l comments = case compileRegex comments of
      Left (column, message) -> Error [CommentRegexError l column message]
      Right _ -> case compileRegex ("((" <> comments <> ")|\\s)*") of
        Left err -> compErr "P1Lexing.Lexer.mkLexer.mkWhitespace" $ show err
        Right reg -> Data reg

-- TODO: catch file not found stuff and put in 'Result'
-- | Construct a new 'Lexer' positioned at the beginning of the given file
startFileLex :: FilePath -> Lexer l n -> IO (Result [Error l n] (Lexer l n))
startFileLex path lexer = do
  source <- ByteString.readFile path
  let utf8res = case decodeUtf8' source of
                  Left err -> Error [UnicodeError err]
                  Right _ -> Data ()
  return $ utf8res *> (Data lexer { remainingSource = source, position = Position 1 1 })

-- | Update position according to the symbols passed in the given string. Assumes the
-- bytestring is properly encoded UTF8.
advancePosition :: ByteString -> Position -> Position
advancePosition str pos = UTF8.foldl step pos str
  where
    step (Position l _) '\n' = Position (l+1) (column firstPosition)
    step (Position l c) _ = Position l (c+1)

-- | Finds the longest token at the current location for each of the supplied languages.
-- If multiple tokens have the same length, prioritize a literal token. If there
-- is still a tie, give an error. The first component is a length of the token. Note
-- that this length includes whitespace, both before and after the token.
lexHere :: (Eq l, Hashable l) => [l] -> Lexer l n -> Result [Error l n] (HashMap l (Maybe (Int, Token l n)))
lexHere langs Lexer{..} = fmap M.fromList . forM langs $ \lang -> do
  let LanguageInternal{..} = compFromJust "P1Lexing.Lexer.lexHere" "missing language" $ M.lookup lang languages
      preceedingWhitespace = runRegex whitespaceRegex remainingSource & fold
      whiteSpaceLength = UTF8.length preceedingWhitespace
      nonWhitespace = ByteString.drop (ByteString.length preceedingWhitespace) remainingSource

      startPos = advancePosition preceedingWhitespace position

      mkRange str = Range startPos $ advancePosition str startPos
      mkLitTok str = (UTF8.length str, LitTok (mkRange str) lang (decodeUtf8 str))
      mkOtherTok (n, str) = (UTF8.length str, OtherTok (mkRange str) lang n (decodeUtf8 str))
      parseOtherTok (n, reg) = (n,) <$> runRegex reg nonWhitespace

      lits = toList literals & filter (`ByteString.isPrefixOf` nonWhitespace) & fmap mkLitTok
      others = toList tokenRegexes & mapMaybe parseOtherTok & fmap mkOtherTok

      whitespaceLengthAfter len = UTF8.drop len >>> runRegex whitespaceRegex >>> fold >>> UTF8.length
      finish (len, tok) = (len + whiteSpaceLength + whitespaceLengthAfter len nonWhitespace, tok)

      litOrOther = case (allMaxByFst lits, allMaxByFst others) of
                     (Nothing, Nothing) -> Nothing
                     (Just (_, ts), Nothing) -> Just ts
                     (Nothing, Just (_, ts)) -> Just ts
                     (Just (litLen, lits'), Just (otherLen, others'))
                       | litLen >= otherLen -> Just lits'
                       | otherwise -> Just others'
  (lang, ) <$> case litOrOther of
    Nothing -> return Nothing
    Just [t] -> return . Just $ finish t
    Just ts -> Error [OverlappingLex lang $ snd <$> ts]
  where
    allMaxByFst :: [(Int, a)] -> Maybe (Int, [(Int, a)])
    allMaxByFst [] = Nothing
    allMaxByFst l = Just $ foldl' step (-1, []) l
    step prev@(prevMax, prevs) new@(newI, _)
      | prevMax > newI = prev
      | prevMax == newI = (prevMax, new : prevs)
      | otherwise = (newI, [new])

-- TODO: there might be a bug when switching between languages towards the end of the file, if there
-- is text left, but it's all whitespace according to the remaining lexer
-- | Advance the position of the lexer by the specified amount of characters. Returns Nothing if
-- the end of the file is reached.
advanceLexer :: Int -> Lexer l n -> Maybe (Lexer l n)
advanceLexer len lex@Lexer{position,remainingSource}
  | future == "" = Nothing
  | otherwise = Just $ lex
    { position = advancePosition past position, remainingSource = future }
  where
    (past, future) = UTF8.splitAt len remainingSource

-- | Convenience function for lexing an entire file as a single language.
allOneLanguage :: forall l n. (Eq l, Hashable l)
               => l -> LanguageTokens n -> FilePath -> IO (Result [Error l n] [Token l n])
allOneLanguage lang langToks path = do
  resLexer <- mkLexer [(lang, langToks)] <&> startFileLex path & sequenceA & fmap join
  return $ resLexer >>= go
  where
    go :: Lexer l n -> Result [Error l n] [Token l n]
    go lex = do
      tokens <- lexHere [lang] lex
      case tokens & M.elems & catMaybes of
        [] -> Error [NoLex $ decodeUtf8 $ remainingSource lex]
        [(len, t)] -> (t:) <$> maybe (return []) go (advanceLexer len lex)
        ts -> compErr "P1Lexing.Lexer.allOneLanguage.go" $ "Impossible: " <> show (length ts)

-- module Regex where

-- import Text.Regex.PCRE.Text (execute)
-- import Text.Regex.PCRE.Wrap (compUTF8, compNoAutoCapture, wrapCompile, Regex)

-- import Data.Text.Encoding (encodeUtf8)
-- import Data.ByteString (useAsCString)

-- safeCompile :: Text -> IO _
-- safeCompile t = useAsCString (encodeUtf8 t) $ \str -> do
--   let compOpts = compUTF8 .|. compNoAutoCapture
--       execOpts = 0
--   wrapCompile compOpts execOpts str >>= \case
--     Left s -> compErr "Regex.safeCompile" $ show s
--     Right reg -> return reg

-- match :: Regex -> Text -> Maybe Text
-- match reg t = case unsafePerformIO (execute reg t) of
--   Left err -> compErr "Regex.match" $ show err
--   Right
