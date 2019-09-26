module P5DynamicAmbiguity.Analysis (analyze, Error, ErrorOptions(..)) where

import Pre

import Text.Printf (printf)
import qualified Data.Text as Text
import qualified Data.HashSet as S
import qualified Data.HashMap.Lazy as M
import Data.Semigroup (Max(..))

import Data.Automaton.NVA (NVA, shortestUniqueWord, shortestWord)
import Data.Automaton.EpsilonNVA (untag, TaggedTerminal(..))

import ErrorMessage (FormatError(..), simpleErrorMessage)

import P1Lexing.Types (Range, textualRange, Ranged)
import P2LanguageDefinition.Types (TypeName, Name(..))
import P4Parsing.Types (NodeF(..))
import P5DynamicAmbiguity.TreeLanguage (treeLanguage, PreLanguage)
import P5DynamicAmbiguity.Types

data Error elidable tok = Ambiguity Range [(NodeOrElide elidable tok, Text)] [NodeOrElide elidable tok]  -- ^ Resolvable alternatives, and unresolvable alternatives

instance FormatError (Error elidable tok) where
  type ErrorOpts (Error elidable tok) = ErrorOptions elidable
  formatError EO{showTwoLevel, elidedRange, showElided} (Ambiguity r resolvable trees) = simpleErrorMessage r $
    kind <> " with " <> show (length trees + length resolvable) <> " alternatives.\n" <>
    resolvableSection <>
    unresolvableSection
    where
      hasResolvable = not $ null resolvable
      hasUnresolvable = not $ null trees
      kind | hasUnresolvable = "Unresolvable ambiguity error"
           | otherwise = "Ambiguity error"
      resolvableSection
        | not hasResolvable = ""
        | hasUnresolvable = "\nResolvable alternatives:\n" <> formattedResolvable
        | otherwise = "\n" <> formattedResolvable
      formattedResolvable
        | showTwoLevel = resolvable
          <&> (twoLevel *** S.singleton)
          & M.fromListWith S.union
          & M.toList
          & foldMap (\(two, ts) -> foldMap (\t -> "  " <> t <> "\n") ts <> two <> "\n")
        | otherwise = resolvable
          & foldMap (\(_, t) -> "  " <> t <> "\n")
      unresolvableSection
        | not hasUnresolvable = ""
        | hasResolvable = "\nUnresolvable alternatives:\n" <> formattedUnresolvable
        | otherwise = "\n" <> formattedUnresolvable
      formattedUnresolvable = trees <&> twoLevel & S.fromList & fold
      twoLevel :: NodeOrElide elidable tok -> Text
      twoLevel (Elide elided) = "  " <> showElided elided <> "\n"
      twoLevel (Node n@NodeF{n_nameF}) = "  " <> coerce n_nameF <> formatChildren (toList n) <> "\n"
        where
          range (Node NodeF{n_rangeF}) = n_rangeF
          range (Elide elided) = elidedRange elided
          formatChildren = sortBy (comparing range) >>> foldMap formatNode
          formatNode (Elide elided) =
            printf "\n   - %- 20s %s" (showElided elided) (textualRange $ elidedRange elided) & Text.pack
          formatNode (Node NodeF{n_nameF = Name name, n_rangeF}) =
            printf "\n   - %- 20s %s" name (textualRange n_rangeF) & Text.pack

data ErrorOptions elidable = EO
  { showTwoLevel :: Bool
  , showElided :: elidable -> Text
  , elidedRange :: elidable -> Range }

type ResLang elidable = NVA Int Int (Token elidable) (Token elidable) (Token elidable)

analyze :: forall elidable tok. (Eq elidable, Hashable elidable, Show elidable, Show tok, Ranged tok)
        => Int  -- ^ Timeout in microseconds. Negative to never timeout.
        -> PreLanguage
        -> (tok -> Token elidable)
        -> (elidable -> (Range, TypeName))
        -> (elidable -> Text)
        -> HashSet (NodeOrElide elidable tok)
        -> IO (Error elidable tok)
analyze timeout pl mkToken getElided showElided alts = do
  res <- shortest
  M.toList languages
    <&> (\(node, lang) ->
           case M.lookup lang res of
             Nothing -> Right node
             Just w -> untag identity identity identity <$> w
               <&> textualToken showElided
               & Text.unwords
               & (node, )
               & Left)
    & partitionEithers
    & uncurry (Ambiguity range)
    & return
  where
    range = toList alts & \case
      Node NodeF{n_rangeF} : _ -> n_rangeF
      Elide elidable : _ -> getElided elidable & fst
      [] -> compErr "P5DynamicAmbiguity.analyze.range" "got zero alternatives"
    mkLanguage = treeLanguage pl mkToken getElided

    languages :: HashMap (NodeOrElide elidable tok) (ResLang elidable)
    languages = S.toMap alts & M.mapWithKey (\node _ -> mkLanguage node)

    shortest :: IO (HashMap (ResLang elidable) [TaggedTerminal (Token elidable) (Token elidable) (Token elidable)])
    shortest = do
      result <- shortestUniqueWord timeout (len + 10) nvas
      S.toMap nvas & M.mapMaybeWithKey (getResult result)
        & return
      where
        Max len = foldMap (shortestWord >>> fmap (length >>> Max) >>> fold) nvas
        nvas = foldMap S.singleton languages
        duplicateLangs = toList languages
          <&> (, Sum @Int 1)
          & M.fromListWith (<>)
          & M.filter (getSum >>> (> 1))
        getResult result nva _
          | M.member nva duplicateLangs = Nothing
          | otherwise = M.lookup nva result
