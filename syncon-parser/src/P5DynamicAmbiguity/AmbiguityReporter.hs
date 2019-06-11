{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module P5DynamicAmbiguity.AmbiguityReporter
( report
, Error(..)
, ErrorOptions(..)
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
import P2LanguageDefinition.Types (Name(..), TypeName(..))
import P4Parsing.Types (pattern Node, n_name)
import qualified P4Parsing.Types as P4
import P4Parsing.Parser (SingleLanguage(..))
import P5DynamicAmbiguity.TreeLanguage (treeLanguage, PreLanguage)
import P5DynamicAmbiguity.Types

data Error
  = Ambiguity Range [(Node, Text)]
  | Unresolvable Range [(Node, Text)] [Node]  -- ^ Resolvable alternatives, and unresolvable alternatives
  deriving (Show)

type Node = P4.Node SingleLanguage TypeName

data ErrorOptions = EO
  { showTwoLevel :: Bool }

-- TODO: pluralize the text below appropriately
instance FormatError Error where
  type ErrorOpts Error = ErrorOptions
  formatError eo (Unresolvable r resolvable trees) = simpleErrorMessage r $
    "Unresolvable ambiguity error with " <> show (length trees + length resolvable) <> " alternatives.\n" <>
    formattedResolvable <> "\n" <>
    "Unresolvable alternatives:\n" <>
    fold unresolvables
    where
      formattedResolvable
        | null resolvable = ""
        | otherwise = "\nResolvable alternatives:\n" <> formatResolvable eo resolvable
      unresolvables = trees
        <&> twoLevel
        & S.fromList
  formatError eo (Ambiguity r resolvable) = simpleErrorMessage r $
    "Ambiguity error with " <> show (length resolvable) <> " alternatives:\n\n" <>
    formatResolvable eo resolvable

twoLevel :: Node -> Text
twoLevel n@Node{n_name} = "  " <> coerce n_name <> formatChildren (project n & toList) <> "\n"
  where
    formatChildren = sortBy (comparing range) >>> foldMap formatNode
    formatNode n'@Node{n_name = Name name} =
      printf "\n   - %- 20s %s" name (textualRange $ range n') & Text.pack

formatResolvable :: ErrorOptions -> [(Node, Text)] -> Text
formatResolvable EO{showTwoLevel} resolvable
  | showTwoLevel = groupedResolvable
    & foldMap (\(two, ts) -> foldMap (\t -> "  " <> t <> "\n") ts <> two <> "\n")
  | otherwise = resolvable
    & foldMap (\(_, t) -> "  " <> t <> "\n")
  where
    groupedResolvable = resolvable
      <&> (twoLevel *** S.singleton)
      & M.fromListWith S.union
      & M.toList

-- | If the argument is a singleton set, return the element, otherwise produce
-- one or more localized ambiguity errors.
report :: PreLanguage -> HashSet Node -> Result [Error] Node
report pl = toList >>> \case
  [] -> compErr "P4Parsing.AmbiguityReporter.report" "Expected one or more parse trees, got zero."
  [top] -> Data top
  forest -> localizeAmbiguities forest
    <&> ((head >>> foldMap range) &&& S.fromList)
    <&> uncurry (resolvability pl)
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

resolvability :: PreLanguage -> Range -> HashSet Node -> Error
resolvability pl r nodes = M.toList languages
  <&> (\(node, lang) ->
         case M.lookup lang shortest of
           Nothing -> Left node
           Just w -> untag identity identity identity <$> w
             & fmap textualToken
             & Text.unwords
             & (node, )
             & Right)
  & partitionEithers
  & \case
    ([], resolvable) -> Ambiguity r resolvable
    (unresolvable, resolvable) -> Unresolvable r resolvable unresolvable
  where
    mkLanguage = treeLanguage pl

    languages :: HashMap Node ResLang
    languages = S.toMap nodes
      & M.mapWithKey (\node _ -> mkLanguage node)

    shortest :: HashMap ResLang [TaggedTerminal Token Token Token]
    shortest = S.toMap nvas & M.mapMaybeWithKey getResult
      where
        Max len = foldMap (shortestWord >>> fmap (length >>> Max) >>> fold) nvas
        nvas = foldMap S.singleton languages
        duplicateLangs = toList languages
          <&> (, Sum @Int 1)
          & M.fromListWith (<>)
          & M.filter (getSum >>> (> 1))
        result = unsafePerformIO $ shortestUniqueWord 1_000_000 (len + 10) nvas
        getResult nva _
          | M.member nva duplicateLangs = Nothing
          | otherwise = M.lookup nva result
