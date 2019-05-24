{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module P5DynamicAmbiguity.AmbiguityReporter
( report
, Error(..)
) where

import Pre hiding (reduce)
import Result (Result(..))

import Text.Printf (printf)
import qualified Data.Text as Text
import qualified Data.HashSet as S
import qualified Data.HashMap.Lazy as M
import Data.Semigroup (Max(..))
import System.IO.Unsafe (unsafePerformIO)

import Data.Functor.Foldable (project)

import Data.Automaton.NVA (NVA, shortestUniqueWord, shortestWord)
import Data.Automaton.EpsilonNVA (untag, TaggedTerminal(..))

import ErrorMessage (FormatError(..), simpleErrorMessage)

import P1Lexing.Types (Range, range, textualRange)
import P2LanguageDefinition.Types (Name(..), TypeName(..), DefinitionFile(..))
import P4Parsing.Types (pattern Node, n_name)
import qualified P4Parsing.Types as P4
import P4Parsing.Parser (SingleLanguage(..))
import P5DynamicAmbiguity.TreeLanguage (treeLanguage)
import P5DynamicAmbiguity.Types

data Error
  = Ambiguity Range [Text]
  | Unresolvable Range [Text] [Node]  -- ^ Resolvable alternatives, and unresolvable alternatives
  deriving (Show)

type Node = P4.Node SingleLanguage TypeName

-- TODO: pluralize the text below appropriately
instance FormatError Error where
  formatError (Unresolvable r resolvable trees) = simpleErrorMessage r $
    "Unresolvable ambiguity error with " <> show (length trees + length resolvable) <> " alternatives.\n" <>
    formattedResolvable <> "\n" <>
    "Unresolvable alternatives:\n" <>
    fold unresolvables
    where
      format (name, children) = "  " <> coerce name <> formatChildren children <> "\n"
      formatChildren = sortBy (compare `on` range) >>> foldMap formatNode
      formatNode n@Node{n_name = Name n'} =
        printf "\n   - %- 20s %s" n' (textualRange $ range n) & Text.pack
      formattedResolvable
        | null resolvable = ""
        | otherwise = "\nResolvable alternatives:\n" <> foldMap (\t -> "  " <> t <> "\n") resolvable
      unresolvables = trees
        <&> (n_name &&& (project >>> toList))
        <&> format
        & S.fromList
  formatError (Ambiguity r resolvable) = simpleErrorMessage r $
    "Ambiguity error with " <> show (length resolvable) <> " alternatives:\n\n" <>
    foldMap (\t -> "  " <> t <> "\n") resolvable

-- | If the argument is a singleton set, return the element, otherwise produce
-- one or more localized ambiguity errors.
report :: DefinitionFile -> HashSet Node -> Result [Error] Node
report df = toList >>> \case
  [] -> compErr "P4Parsing.AmbiguityReporter.report" "Expected one or more parse trees, got zero."
  [top] -> Data top
  forest -> localizeAmbiguities forest
    <&> ((head >>> foldMap range) &&& S.fromList)
    <&> uncurry (resolvability df)
    & Error

localizeAmbiguities :: (Eq l, Eq n) => [P4.Node l n] -> [[P4.Node l n]]
localizeAmbiguities forest
  | equalBy (project >>> void) forest
  , let subforests = transpose $ map (project >>> toList) forest
  , all (equalBy range) subforests
    = concatMap localizeAmbiguities subforests
  | otherwise = [forest]

-- |
-- = Parse-time resolvability

type ResLang = NVA Int Int Token Token Token

resolvability :: DefinitionFile -> Range -> HashSet Node -> Error
resolvability df r nodes = M.toList languages
  <&> (\(node, lang) ->
         case M.lookup lang shortest of
           Nothing -> Left node
           Just w -> untag identity identity identity <$> w
             & fmap textualToken
             & Text.unwords
             & Right)
  & partitionEithers
  & \case
    ([], resolvable) -> Ambiguity r resolvable
    (unresolvable, resolvable) -> Unresolvable r resolvable unresolvable
  where
    mkLanguage = treeLanguage df

    languages :: HashMap Node ResLang
    languages = S.toMap nodes
      & M.mapWithKey (\node _ -> mkLanguage node)

    shortest :: HashMap ResLang [TaggedTerminal Token Token Token]
    shortest = unsafePerformIO $ shortestUniqueWord 1_000_000 (len + 10) nvas
      where
        Max len = foldMap (shortestWord >>> fmap (length >>> Max) >>> fold) nvas
        nvas = foldMap S.singleton languages
