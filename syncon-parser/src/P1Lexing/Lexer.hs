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
, allOneLanguage'
) where

import Pre
import Result (Result(..))

import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Array as Array
import qualified Data.Text as Text
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as M

import ErrorMessage (FormatError(..), simpleErrorMessage)

import Text.Regex.PCRE.ByteString (compile, execute, Regex, compUTF8, compNoAutoCapture)

import P1Lexing.Types

data LanguageTokens n = LanguageTokens
  [Text] -- ^ Literals
  [(n, (Range, Text))] -- ^ Other token kinds, with regexes in the texts (paired with the range at which they were defined)
  [(Range, Text)] -- ^ Regexes for comments (range for the string, the regex itself)

data Error l n
  = RegexError l Text Position Text
  | UnicodeError UnicodeException
  | OverlappingLex l [Token l n]
  | NoLex Text Position
  deriving (Show)

instance (Show l, Show n) => FormatError (Error l n) where
  formatError (RegexError _ f pos t) = simpleErrorMessage (Range f pos pos) $
    "Regex error: " <> t
  formatError (UnicodeError ex) = simpleErrorMessage Nowhere $
    "The file must be UTF8 encoded, got an error: " <> show ex
  formatError (OverlappingLex _ toks) = simpleErrorMessage (foldMap range toks) $
    "Lexing needs to produce a unique longest token, but there were multiple:\n"
    <> foldMap (show >>> (<> "\n")) toks
  formatError (NoLex f pos) = simpleErrorMessage (Range f pos (Position (line pos + 4) maxBound)) $
    "There is no valid token starting here."

data LanguageInternal n = LanguageInternal
  { literals :: !(Seq ByteString)
  , tokenRegexes :: !(Seq (n, Regex))
  , whitespaceRegex :: !Regex
  }

data Lexer l n = Lexer
  { remainingSource :: !ByteString
  , file :: !Text
  , position :: !Position
  , languages :: !(HashMap l (LanguageInternal n))
  }

-- | Run a previously compiled regex. Assumes the foreign call will succeed, e.g., that there
-- is enough memory, uses 'compErr' otherwise.
runRegex :: Regex -> ByteString -> Maybe ByteString
runRegex reg str = case unsafePerformIO (execute reg str) of
  Left err -> compErr "P1Lexing.Lexer.runRegex" $ show err
  Right Nothing -> Nothing
  Right (Just arr) -> case arr Array.! fst (Array.bounds arr) of
    (start, len) -> str & ByteString.drop start & ByteString.take len & Just

-- | Compile a regex, then get either a column and error message or a compiled regex.
compileRegex :: Text -> Either (Int, Text) Regex
compileRegex t = unsafePerformIO (compile compOpts execOpts (encodeUtf8 ("(*UCP)^" <> t))) & first (second toS)
  where
    compOpts = compUTF8 .|. compNoAutoCapture
    execOpts = 0

-- | Wrapper around 'compileRegex' that produces a nice 'Error' for regexes where we have a 'Range'
-- where they were defined.
compileRegex' :: l -> Range -> Text -> Result [Error l n] Regex
compileRegex' l r t = case compileRegex t of
  Left (errColumn, message) -> case r of
    Range f (Position line column) _ -> Error [RegexError l f (Position line (column + errColumn)) (toS message)]
    Nowhere -> Error [RegexError l "" (Position (-1) errColumn) (toS message)]
  Right regex -> Data regex

-- | Construct a new lexer, capable of lexing multiple languages
mkLexer :: forall l n. (Eq l, Hashable l) => [(l, LanguageTokens n)] -> Result [Error l n] (Lexer l n)
mkLexer langs = do
  langs' <- traverse mkLang langs
  return $ Lexer
    { remainingSource = ""
    , position = Position 1 1
    , file = ""
    , languages = M.fromList langs' }
  where
    mkLang :: (l, LanguageTokens n) -> Result [Error l n] (l, LanguageInternal n)
    mkLang (l, LanguageTokens lits others comments) = do
      let lits' = Seq.fromList $ encodeUtf8 <$> lits
      others' <- Seq.fromList <$> traverse (mkRegex l) others
      whitespace <- mkWhitespace l comments
      return . (l,) $ LanguageInternal
        { literals = lits'
        , tokenRegexes = others'
        , whitespaceRegex = whitespace }

    -- Produce a regex that parses whitespace
    mkWhitespace _ [] = case compileRegex "\\s*" of
      Left err -> compErr "P1Lexing.Lexer.mkLexer.mkWhitespace" $ show err
      Right reg -> Data reg
    mkWhitespace l comments = traverse (uncurry $ compileRegex' l) comments >>= \_ ->
      case compileRegex $ "(" <> Text.intercalate "|" (fmap (snd >>> paren) comments) <> "|\\s)*" of
        Left err -> compErr "P1Lexing.Lexer.mkLexer.mkWhitespace" $ show err
        Right reg -> Data reg

    mkRegex l (n, (r, t)) = (n,) <$> compileRegex' l r t
    paren s = "(" <> s <> ")"

-- TODO: catch file not found stuff and put in 'Result'
-- | Construct a new 'Lexer' positioned at the beginning of the given file.
startFileLex :: FilePath -> Lexer l n -> IO (Result [Error l n] (Lexer l n))
startFileLex path lexer = do
  source <- ByteString.readFile path
  let utf8res = case decodeUtf8' source of
                  Left err -> Error [UnicodeError err]
                  Right _ -> Data ()
  return $ utf8res *>
    (Data lexer { remainingSource = source
                , position = Position 1 1
                , file = toS path })

-- | Update position according to the symbols passed in the given string. Assumes the
-- bytestring is properly encoded UTF8.
advancePosition :: ByteString -> Position -> Position
advancePosition str pos = UTF8.foldl stepPosition pos str

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

      mkRange str = Range file startPos $ advancePosition str startPos
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

-- | Convenience function for producing a function that lexes any file with a single language.
-- See also 'allOneLanguage''.
allOneLanguage :: forall l n. (Eq l, Hashable l)
               => l -> LanguageTokens n
               -> Result [Error l n] (FilePath -> IO (Result [Error l n] [Token l n]))
allOneLanguage lang langToks = do
  lexer <- mkLexer [(lang, langToks)]
  return $ \path -> fmap (>>= go) $ startFileLex path lexer
  where
    go :: Lexer l n -> Result [Error l n] [Token l n]
    go lex = do
      tokens <- lexHere [lang] lex
      case tokens & M.elems & catMaybes of
        [] -> Error [NoLex (file lex) (position lex)]
        [(len, t)] -> (t:) <$> maybe (return []) go (advanceLexer len lex)
        ts -> compErr "P1Lexing.Lexer.allOneLanguage.go" $ "Impossible: " <> show (length ts)

-- | Convenience function for parsing a single file using a single language.
allOneLanguage' :: (Eq l, Hashable l)
                => l -> LanguageTokens n -> FilePath
                -> IO (Result [Error l n] [Token l n])
allOneLanguage' lang langToks path = allOneLanguage lang langToks <*> pure path
  & sequenceA & fmap join
