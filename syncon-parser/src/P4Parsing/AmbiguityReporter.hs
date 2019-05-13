module P4Parsing.AmbiguityReporter
( report
, Error(..)
) where

import Pre
import Result (Result(..))

import Text.Printf (printf)
import qualified Data.Text as Text
import qualified Data.HashSet as S

import Data.Functor.Foldable (project)

import ErrorMessage (FormatError(..), simpleErrorMessage)

import P1Lexing.Types (Range, range, textualRange)
import P2LanguageDefinition.Types (Name(..), Syncon)
import P4Parsing.Types

data Error l n
  = Ambiguity Range (HashSet (Node l n))
  deriving (Show)

instance FormatError (Error l n) where
  formatError (Ambiguity r trees) = simpleErrorMessage r $
    "Ambiguity error with " <> show (length trees) <> " alternatives.\n" <>
    foldMap ((n_name &&& (project >>> toList)) >>> format) trees
    where
      format (name, children) = coerce name <> formatChildren children <> "\n"
      formatChildren = sortBy (compare `on` range) >>> foldMap formatNode
      formatNode n@Node{n_name = Name n'} =
        printf "\n - %- 20s %s" n' (textualRange $ range n) & Text.pack

-- | If the argument is a singleton set, return the element, otherwise produce
-- one or more localized ambiguity errors.
report :: (Eq l, Hashable l, Eq n, Hashable n)
       => HashMap Name Syncon -> HashSet (Node l n) -> Result [Error l n] (Node l n)
report _syncons = toList >>> \case
  [] -> compErr "P4Parsing.AmbiguityReporter.report" "Expected one or more parse trees, got zero."
  [top] -> Data top
  forest -> localizeAmbiguities forest
    <&> ((head >>> foldMap range) &&& S.fromList)
    <&> uncurry Ambiguity
    & Error

localizeAmbiguities :: (Eq l, Eq n) => [Node l n] -> [[Node l n]]
localizeAmbiguities forest
  | equalBy (project >>> void) forest
  , let subforests = transpose $ map (project >>> toList) forest
  , all (equalBy range) subforests
    = concatMap localizeAmbiguities subforests
  | otherwise = [forest]

-- |
-- = Parse-time resolvability

-- data Token l n
--   = LitTok l Text
--   | OtherTokInstance l n Text
--   | OtherTok l n

-- mkLanguage :: HashMap Name Syncon -> Node l n -> DVA
