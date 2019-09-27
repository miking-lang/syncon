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
import Data.Char (isSpace)
import Codec.Serialise (Serialise)
import qualified Data.Array as Array
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as M

import ErrorMessage (FormatError(..), simpleErrorMessage, ErrorMessage(..))

import Text.Regex.PCRE.ByteString (compile, execute, Regex, compUTF8, compNoAutoCapture)

import P1Lexing.Types

data LanguageTokens n = LanguageTokens
  [Text] -- ^ Literals
  [(n, (Range, Text))] -- ^ Other token kinds, with regexes in the texts (paired with the range at which they were defined)
  [((Range, Text), (Range, Text))] -- ^ Regexes for comments (range for the string, the regex itself). The regexes are presented as 'begin' 'end' pairs.
  deriving (Generic)
instance NFData n => NFData (LanguageTokens n)
instance Serialise n => Serialise (LanguageTokens n)

data Error l n
  = RegexError l Text Position Text
  | UnicodeError UnicodeException
  | OverlappingLex l [Token l n]
  | NoLex Text Position
  | AmbiguousCommentStart l [Range]
  | UnclosedBlockComment l Range  -- ^ The range points to the opening bracket of the block comment, not the entire rest of the file
  deriving (Show)

instance (Show l, Show n) => FormatError (Error l n) where
  type ErrorOpts (Error l n) = ()
  formatError _ (RegexError _ f pos@(Position l c) t) = simpleErrorMessage (Range f pos $ Position l (c+1)) $
    "Regex error: " <> t
  formatError _ (UnicodeError ex) = simpleErrorMessage Nowhere $
    "The file must be UTF8 encoded, got an error: " <> show ex
  formatError _ (OverlappingLex _ toks) = simpleErrorMessage (foldMap range toks) $
    "Lexing needs to produce a unique longest token, but there were multiple:\n"
    <> foldMap (show >>> (<> "\n")) toks
  formatError _ (NoLex f pos) = simpleErrorMessage (Range f pos (Position (line pos + 4) maxBound)) $
    "There is no valid token starting here."
  formatError _ (AmbiguousCommentStart _ rs) = ErrorMessage
    { e_message = "Ambiguous comment start, this could be the beginning of multiple different kinds of block comments."
    , e_range = fold rs
    , e_ranges = (, "") <$> rs
    }
  formatError _ (UnclosedBlockComment _ r) = simpleErrorMessage r $
    "Unclosed block comment."

data LanguageInternal n = LanguageInternal
  { literals :: !(Seq ByteString)
  , tokenRegexes :: !(Seq (n, Regex))
  , commentRegexes :: !(Seq (Regex, (Regex, Regex)))  -- ^ (begin at start of string, begin anywhere, end anywhere)
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

-- | Run a previously compiled regex. Assumes the foreign call will suceed, e.g., that there
-- is enough memory, uses 'compErr' otherwise. Returns a tuple of (start index, length), expressed
-- in bytes.
runRegex' :: Regex -> ByteString -> Maybe (Int, Int)
runRegex' reg str = case unsafePerformIO (execute reg str) of
  Left err -> compErr "P1Lexing.Lexer.runRegex'" $ show err
  Right Nothing -> Nothing
  Right (Just arr) -> Just $ arr Array.! fst (Array.bounds arr)

-- | Compile a regex, then get either a column and error message or a compiled regex.
compileRegex :: Text -> Either (Int, Text) Regex
compileRegex t = unsafePerformIO (compile compOpts execOpts (encodeUtf8 ("(*UCP)" <> t))) & first (second toS)
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
      comments' <- Seq.fromList <$> traverse (mkComment l) comments
      return . (l,) $ LanguageInternal
        { literals = lits'
        , tokenRegexes = others'
        , commentRegexes = comments' }

    -- Produce a regex that parses whitespace
    mkComment l (begin, end) = do
      beginAnywhere <- compileRegex' l `uncurry` begin
      let beginStrict = case snd begin & paren & ("^" <>) & compileRegex of
            Left e -> compErr "P1Lexing.Lexer.mkLexer.mkComment" $ "Couldn't compile begin comment when prefixed with ^: " <> show (snd begin) <> ", error: " <> show e
            Right reg -> reg
      endAnywhere <- compileRegex' l `uncurry` end
      pure $ (beginStrict, (beginAnywhere, endAnywhere))

    mkRegex l (n, (r, t)) = (n,) <$> compileRegex' l r ("^" <> paren t)

    paren t = "(" <> t <> ")"

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

-- | Consume the leading whitespace according to the given language. Returns a tuple of
-- (leading whitespace, remaining source).
consumeLeadingWhitespace :: forall l n. l
                         -> LanguageInternal n
                         -> (Text, Position)
                         -> ByteString
                         -> Result [Error l n] (ByteString, ByteString)
consumeLeadingWhitespace l LanguageInternal{commentRegexes} (path, startPos) fullStr =
  dropWhitespace startPos fullStr
  & dropAllComments
  <&> \rest ->
        (ByteString.take (ByteString.length fullStr - ByteString.length rest) fullStr, rest)
  where
    dropWhitespace pos = UTF8.span isSpace >>> first (flip advancePosition pos)

    dropAllComments :: (Position, ByteString) -> Result [Error l n] ByteString
    dropAllComments (pos, str) = dropOneComment pos str >>= \case
      Nothing -> pure str
      Just rest ->
        dropWhitespace
          (advancePosition (ByteString.take (ByteString.length rest - ByteString.length str) str) pos)
          rest
        & dropAllComments

    dropOneComment :: Position -> ByteString -> Result [Error l n] (Maybe ByteString)
    dropOneComment pos str = case mapMaybe mkStart $ toList commentRegexes of
      [] -> pure Nothing
      [(openRange, (rest, pair))] -> case dropComments pair 1 rest of
        Nothing -> Error [UnclosedBlockComment l openRange]
        Just postComment -> pure $ Just postComment
      opens -> opens <&> fst & AmbiguousCommentStart l & pure & Error
      where
        mkStart :: (Regex, (Regex, Regex)) -> Maybe (Range, (ByteString, (Regex, Regex)))
        mkStart (reg, pair) = do
          (st, len) <- runRegex' reg str
          let (openBracket, rest) = ByteString.splitAt (st + len) str
              endPos = advancePosition openBracket pos
          pure (Range path pos endPos, (rest, pair))

    -- | Drop nested comments appropriately. The 'Int' is the nesting level, starts at 1
    -- for a newly opened comment. Returns 'Nothing' if that first comment is never closed.
    dropComments :: (Regex, Regex) -> Int -> ByteString -> Maybe ByteString
    dropComments _ level str
      | level <= 0 = Just str
    dropComments regs@(openReg, closeReg) level str = str
      & (runRegex' openReg &&& runRegex' closeReg)
      & \case
      (Nothing, Nothing) -> Nothing
      (Just (sIdx, len), Nothing) -> dropComments regs (level+1) $ ByteString.drop (sIdx + len) str
      (Nothing, Just (sIdx, len)) -> dropComments regs (level-1) $ ByteString.drop (sIdx + len) str
      (Just (sIdx, len), Just (s2Idx, _))
        | sIdx < s2Idx -> dropComments regs (level+1) $ ByteString.drop (sIdx + len) str
      (Just _, Just (sIdx, len)) -> dropComments regs (level-1) $ ByteString.drop (sIdx + len) str

-- | Finds the longest token at the current location for each of the supplied languages.
-- If multiple tokens have the same length, prioritize a literal token. If there
-- is still a tie, give an error. The first component is a length of the token. Note
-- that this length includes whitespace, both before and after the token.
lexHere :: (Eq l, Hashable l) => [l] -> Lexer l n -> Result [Error l n] (HashMap l (Maybe (Int, Token l n)))
lexHere langs Lexer{..} = fmap M.fromList . forM langs $ \lang -> do
  let li@LanguageInternal{..} = compFromJust "P1Lexing.Lexer.lexHere" "missing language" $ M.lookup lang languages

  (preceedingWhitespace, nonWhitespace) <- consumeLeadingWhitespace lang li (file, position) remainingSource

  let whiteSpaceLength = UTF8.length preceedingWhitespace

      startPos = advancePosition preceedingWhitespace position

      mkRange str = Range file startPos $ advancePosition str startPos
      mkLitTok str = (UTF8.length str, LitTok (mkRange str) lang (decodeUtf8 str))
      mkOtherTok (n, str) = (UTF8.length str, OtherTok (mkRange str) lang n (decodeUtf8 str))
      parseOtherTok (n, reg) = (n,) <$> runRegex reg nonWhitespace

      lits = toList literals & filter (`ByteString.isPrefixOf` nonWhitespace) & fmap mkLitTok
      others = toList tokenRegexes & mapMaybe parseOtherTok & fmap mkOtherTok

      whitespaceLengthAfter len = UTF8.drop len nonWhitespace
        & consumeLeadingWhitespace lang li (file, position)
        <&> fst
        <&> UTF8.length
      finish (len, tok) = whitespaceLengthAfter len <&> \afterLen ->
        (len + whiteSpaceLength + afterLen, tok)

      litOrOther = case (allMaxByFst lits, allMaxByFst others) of
                     (Nothing, Nothing) -> Nothing
                     (Just (_, ts), Nothing) -> Just ts
                     (Nothing, Just (_, ts)) -> Just ts
                     (Just (litLen, lits'), Just (otherLen, others'))
                       | litLen >= otherLen -> Just lits'
                       | otherwise -> Just others'
  (lang, ) <$> case litOrOther of
    Nothing -> return Nothing
    Just [t] -> finish t <&> Just
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
