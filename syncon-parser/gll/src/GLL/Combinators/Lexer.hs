
module GLL.Combinators.Lexer (
    default_lexer, lexer, LexerSettings(..), emptyLanguage,
    oneOf, manyOf, someOf, baseToDec,
    ) where

import Prelude

import GLL.Types.Grammar (Token(..), SubsumesToken(..))
import Data.List (isPrefixOf)
import Data.Char (isSpace, isDigit, isAlpha, isUpper, isLower)
import Text.Regex.Applicative
import Text.Regex.Applicative.Common (signed)

-- | Settings for changing the behaviour of the builtin lexer 'lexer'.
-- Lexers are built using "Text.Regex.Applicative".
data LexerSettings = LexerSettings {
        -- | Which keychars to recognise? Default: none.
        keychars        :: [Char]
        -- | Which keywords to recognise? Default: none.
    ,   keywords        :: [String]
        -- | What is considered a whitespace character? Default: 'Data.Char.isSpace'.
    ,   whitespace      :: Char -> Bool
        -- | How does a line comment start? Default: '"'//'"'.
    ,   lineComment     :: String
        -- | How does a block comment open? Default: '"'{-'"'.
    ,   blockCommentOpen :: String
        -- | How does a block comment close? Default: '"'-}'"'.
    ,   blockCommentClose :: String
        -- | How to recognise identifiers? Default alphanumerical with lowercase alpha start.
    ,   identifiers     :: RE Char String
        -- | How to recognise alternative identifiers? Default alphanumerical with uppercase alpha start.
    ,   altIdentifiers  :: RE Char String
        -- | Arbitrary tokens /(a,b)/. /a/ is the token name, /b/ is a regular expression.
    ,   tokens          :: [(String, RE Char String)]
        -- | Whether integer literals may be signed positive or negative. Default: 'False'
    ,   signed_int_lits :: Bool
    }

-- | The default 'LexerSettings'.
emptyLanguage :: LexerSettings
emptyLanguage = LexerSettings [] [] isSpace "//" "{-" "-}"
    ((:) <$> psym isLower <*> lowercase_id)
    ((:) <$> psym isUpper <*> lowercase_id)
    [] False
 where lowercase_id = many (psym (\c -> isAlpha c || c == '_' || isDigit c))

-- | A lexer using the default 'LexerSettings'.
default_lexer :: SubsumesToken t => String -> [t]
default_lexer = lexer emptyLanguage

-- | A lexer parameterised by 'LexerSettings'.
lexer :: SubsumesToken t => LexerSettings -> String -> [t]
lexer _ [] = []
lexer lexsets s
  | start /= "" && end /= "" && start `isPrefixOf` s = blockState 1 (drop lS s)
  | lComm /= "" && lComm `isPrefixOf` s = case dropWhile ((/=) '\n') s of
      []      -> []
      (c:cs)  -> lexer lexsets cs
  | isWS (head s) = lexer lexsets (dropWhile isWS s)
  | otherwise = case findLongestPrefix (lTokens lexsets) s of
        Just (tok, rest)   -> tok : lexer lexsets rest
        Nothing            -> error ("lexical error at: " ++ show (take 10 s))
  where start = blockCommentOpen lexsets
        end   = blockCommentClose lexsets
        isWS  = whitespace lexsets
        lComm = lineComment lexsets
        lS    = length start
        lE    = length end

        blockState :: SubsumesToken t => Int -> String -> [t]
        blockState n [] = []
        blockState 0 rest = lexer lexsets rest
        blockState n cs | start `isPrefixOf` cs = blockState (n+1) (drop lS cs)
                    | end `isPrefixOf` cs   = blockState (n-1) (drop lE cs)
                    | otherwise             = blockState n (tail cs)

lTokens :: SubsumesToken t => LexerSettings -> RE Char t
lTokens lexsets =
        lCharacters
    <|> lKeywords
    <|> upcast . IntLit . Just <$> lIntegers (signed_int_lits lexsets)
    <|> upcast . FloatLit . Just <$> lFloats
    <|> upcast . IDLit . Just <$> identifiers lexsets
    <|> upcast . AltIDLit . Just <$> altIdentifiers lexsets
    <|> upcast . CharLit . Just <$> lCharLit
    <|> upcast . StringLit . Just <$> lStringLit
    <|> lMore
  where     lMore = foldr ((<|>) . uncurry lToken) empty (tokens lexsets)

            lChar c = upcast (Char c) <$ sym c
            lCharacters = foldr ((<|>) . lChar) empty (keychars lexsets)

            lKeyword k  = upcast (Keyword k) <$ string k
            lKeywords = foldr ((<|>) . lKeyword) empty (keywords lexsets)

lToken t re = upcast . Token t . Just <$> re

lStringLit = toString <$ sym '\"' <*> many strChar <* sym '\"'
 where strChar =  sym '\\' *> sym '\"'
                  <|> psym ((/=) '\"')
       toString inner = read ("\"" ++ inner ++ "\"")

lCharLit = id <$ sym '\'' <*> charChar <* sym '\''
  where charChar = sym '\\' *> sym '\''
                    <|> psym ((/=) '\'')

lFloats :: RE Char Double
lFloats = signed ( read <$> (
        mkDP <$> decimal <*> sym '.' <*> decimal <*> optional exponent
    <|> mkEP <$> decimal <*> exponent
  ))
  where mkDP pre _ post mexp = pre ++ "." ++ post ++ maybe "" id mexp
        mkEP pre exp = pre ++ exp

        exponent = mk <$> (sym 'e' <|> sym 'E')
                      <*> optional (sym '+' <|> sym '-')
                      <*> decimal
         where mk pre sign dec = pre : maybe "" (:[]) sign ++ dec

lIntegers :: Bool -> RE Char Int
lIntegers True = signed lNaturals
lIntegers False = lNaturals

lNaturals :: RE Char Int
lNaturals =
      (read <$> decimal)
  <|> (baseToDec 16 <$ hexPrefix <*> someOf (['0'..'9']++['A'..'F']++['a'..'f']))
  <|> (baseToDec 8  <$ octPrefix <*> someOf ['0'..'7'])
  <|> (baseToDec 2  <$ binPrefix <*> someOf ['0','1'])
  where hexPrefix = string "0x" <|> string "0X"
        octPrefix = string "0o" <|> string "0O"
        binPrefix = string "0b" <|> string "0B"

decimal :: RE Char String
decimal = someOf ['0'..'9']

-- | Convert numerical representation in a given base
--  (max base = 16, written as string)
--  into decimal representation (returned as Int)
baseToDec :: Int -> String -> Int
baseToDec base = baseToDec' 0 base . map toInt
  where baseToDec' acc base [] = acc
        baseToDec' acc base (d:ds) = baseToDec' (acc * base + d) base ds
        toInt c | c == 'A' || c == 'a' = 10
                | c == 'B' || c == 'b' = 11
                | c == 'C' || c == 'c' = 12
                | c == 'D' || c == 'd' = 13
                | c == 'E' || c == 'e' = 14
                | c == 'F' || c == 'f' = 15
                | otherwise = read [c]

oneOf :: Eq t => [t] -> RE t t
oneOf ts = psym (\t -> t `elem` ts)

manyOf :: Eq t => [t] -> RE t [t]
manyOf ts = many (oneOf ts)

someOf :: Eq t => [t] -> RE t [t]
someOf ts = some (oneOf ts)

{-

manyOf :: Eq t => [t] -> RE t [t]
manyOf ts = empty <|> someOf ts

someOf :: Eq t => [t] -> RE t [t]
someOf ts = (:) <$> oneOf ts <*> manyOf ts

-}
