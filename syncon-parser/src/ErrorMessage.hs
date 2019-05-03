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

import System.Console.Pretty (style, Style(Bold), color, Color(Red))
import Text.Show.Pretty (ppShow)

import P1Lexing.Types (Range(..), Ranged(..), Position(..), firstPosition)

data ErrorMessage = ErrorMessage
  { e_range :: !Range  -- ^ The location of the error. Will not be shown, but will be used for sorting
  , e_ranges :: ![(Range, Text)]  -- ^ The ranges to highlight to the user, and a message for each
  , e_message :: !Text  -- ^ The error message to be displayed to the user
  }

class FormatError a where
  formatError :: a -> ErrorMessage

instance {-# OVERLAPPABLE #-} Show e => FormatError e where
  formatError e = ErrorMessage { e_message = ppShow e & toS, e_range = Nowhere, e_ranges = mempty }

-- | A simpler constructor for error messages that pertain to a single 'Range'.
simpleErrorMessage :: Range -> Text -> ErrorMessage
simpleErrorMessage r t = ErrorMessage { e_message = t, e_range = r, e_ranges = [(r, "")] }

-- TODO: add underline highlighting if the range only covers one line, since that is likely to be a really common case
-- TODO: omit parts of the code if it's a large number of lines
-- | Produce (colored) output suitable for printing the errors. Will extract source code and highlight the
-- relevant section.
formatErrors :: Foldable f => Text -> f ErrorMessage -> Text
formatErrors source messages = sortedMessages <&> formatSingle & Text.intercalate "\n--------------------\n\n"
  where
    sortedMessages :: [ErrorMessage]
    sortedMessages = toList messages & sortBy (compare `on` range)
    firstLine :: Int
    firstLine = line firstPosition
    lines :: Seq (Int, Text)
    lines = [firstLine..] `zip` split (== '\n') source & Seq.fromList

    -- Format a single ErrorMessage
    formatSingle :: ErrorMessage -> Text
    formatSingle ErrorMessage{..} = ensureLn e_message <> foldMap formatRange e_ranges

    -- Format code extracts
    formatRange :: (Range, Text) -> Text
    formatRange (Nowhere, "") = ""
    formatRange (r, "") = "\n" <> extractRange r
    formatRange (r, t) = "\n" <> ensureLn t <> extractRange r

    extractRange :: Range -> Text
    extractRange Nowhere = ""
    extractRange pos@(Range (Position l1 _) (Position l2 _))
      | l1 == l2 = Seq.lookup (l1-firstLine) lines
        & errJust "line out of bounds"
        & formatLine pos
        & ensureLn
      | l1 < l2 = Seq.drop (l1-firstLine) lines & Seq.take (l2-l1+1)
        & fmap (formatLine pos)
        & toList
        & foldMap (<> "\n")
      | otherwise = compErr "ErrorMessage.formatErrors.extractRange" $ "Backwards range: " <> show pos

    formatLine :: Range -> (Int, Text) -> Text
    formatLine Nowhere _ = compErr "ErrorMessage.formatErrors.formatLine" "Unexpected Nowhere"
    formatLine (Range (Position l1 c1) (Position l2 c2)) (lnum, lcontents)
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
