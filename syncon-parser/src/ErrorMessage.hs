{-# LANGUAGE RecordWildCards, UndecidableInstances #-}

module ErrorMessage
( ErrorMessage(..)
, simpleErrorMessage
, FormatError(..)
, formatErrors
) where

import Pre

import Text.Printf (printf)
import qualified Data.Sequence as Seq
import Data.Text (split, splitAt)
import qualified Data.Text as Text
import qualified Data.HashMap.Lazy as M

import System.Console.Pretty (style, Style(Bold), color, Color(Red))
import Text.Show.Pretty (ppShow)

import P1Lexing.Types (Range(..), Ranged(..), Position(..), firstPosition)

data ErrorMessage = ErrorMessage
  { e_range :: !Range  -- ^ The location of the error. Will not be shown, but will be used for sorting
  , e_ranges :: ![(Range, Text)]  -- ^ The ranges to highlight to the user, and a message for each
  , e_message :: !Text  -- ^ The error message to be displayed to the user
  }

class FormatError a where
  type ErrorOpts a
  type ErrorOpts a = ()
  formatError :: ErrorOpts a -> a -> ErrorMessage
  default formatError :: Show a => ErrorOpts a -> a -> ErrorMessage
  formatError _ e = ErrorMessage { e_message = ppShow e & toS, e_range = Nowhere, e_ranges = mempty }

-- | A simpler constructor for error messages that pertain to a single 'Range'.
simpleErrorMessage :: Range -> Text -> ErrorMessage
simpleErrorMessage r t = ErrorMessage { e_message = t, e_range = r, e_ranges = [(r, "")] }

-- TODO: add underline highlighting if the range only covers one line, since that is likely to be a really common case
-- TODO: omit parts of the code if it's a large number of lines
-- | Produce (colored) output suitable for printing the errors. Will extract source code and highlight the
-- relevant section.
formatErrors :: Foldable f => HashMap Text Text -> f ErrorMessage -> Text
formatErrors sources messages = sortedMessages <&> formatSingle & Text.intercalate "\n--------------------\n\n"
  where
    sortedMessages :: [ErrorMessage]
    sortedMessages = toList messages & sortBy (compare `on` range)
    firstLine :: Int
    firstLine = line firstPosition
    lines :: HashMap Text (Seq (Int, Text))
    lines = sources <&> \source -> [firstLine..] `zip` split (== '\n') source & Seq.fromList
    getLines :: Text -> Seq (Int, Text)
    getLines path = M.lookup path lines
      & errJust "missing file"

    -- Format a single ErrorMessage
    formatSingle :: ErrorMessage -> Text
    formatSingle ErrorMessage{..} = ensureLn e_message <> foldMap formatRange e_ranges

    -- Format code extracts
    formatRange :: (Range, Text) -> Text
    formatRange (Nowhere, "") = ""
    formatRange (r, "") = "\n" <> formatPath r <> extractRange r
    formatRange (r, t) = "\n" <> ensureLn t <> formatPath r <> extractRange r

    formatPath (Range f Position{line=l1,column=c1} Position{line=l2,column=c2}) =
      " " <> f <> " " <> show l1 <> ":" <> show c1 <> "-" <> show l2 <> ":" <> show c2 <> "\n"
    formatPath Nowhere = ""

    extractRange :: Range -> Text
    extractRange Nowhere = ""
    extractRange pos@(Range f (Position l1 _) (Position l2 _))
      | l1 == l2 = Seq.lookup (l1-firstLine) (getLines f)
        & errJust "line out of bounds"
        & formatLine pos
        & ensureLn
      | l1 < l2 = Seq.drop (l1-firstLine) (getLines f) & Seq.take (l2-l1+1)
        & fmap (formatLine pos)
        & toList
        & foldMap (<> "\n")
      | otherwise = compErr "ErrorMessage.formatErrors.extractRange" $ "Backwards range: " <> show pos

    formatLine :: Range -> (Int, Text) -> Text
    formatLine Nowhere _ = compErr "ErrorMessage.formatErrors.formatLine" "Unexpected Nowhere"
    formatLine (Range _ (Position l1 c1) (Position l2 c2)) (lnum, lcontents)
      | lnum == l1 && lnum == l2 = formatNumber lnum <> ": " <> highlightBetween c1 c2 lcontents
      | lnum == l1 = formatNumber lnum <> ": " <> highlightBetween c1 maxBound lcontents
      | lnum == l2 = formatNumber lnum <> ": " <> highlightBetween 1 c2 lcontents
      | l1 < lnum && lnum < l2 = formatNumber lnum <> ": " <> highlightBetween 1 maxBound lcontents
      | otherwise = formatNumber lnum <> ": " <> lcontents
    highlightBetween start end text = splitAt (start-1) text
      & fmap (splitAt (end - start))
      & (\(e1, (e2, e3)) -> e1 <> (color Red $ style Bold e2) <> e3)
    formatNumber = printf "% 4d" >>> Text.pack

    -- Misc helpers
    ensureLn "" = ""
    ensureLn t
      | Text.last t == '\n' = t
      | otherwise = t <> "\n"
    errJust = compFromJust "ErrorMessage.formatErrors"

instance Ranged ErrorMessage where
  range = e_range
