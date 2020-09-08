module P5DynamicAmbiguity.Analysis
( analyze
, DynAnalysisKind(..)
, DynConfig(..)
, Error
, ErrorOptions(..)
, ambiguityStyle
, AmbiguityStyle(..)
, didTimeout
, countReparseFailures
, forceResolutions
) where

import Pre hiding (reduce)

import Text.Printf (printf)
import qualified Data.Text as Text
import qualified Data.HashSet as S
import qualified Data.HashMap.Lazy as M
import qualified Data.Sequence as Seq
import Data.Semigroup (Max(..))

import Data.Automaton.NVA (NVA, shortestUniqueWord, shortestWord, unions, reduce, TimeoutInfo(..))
import Data.Automaton.EpsilonNVA (untag, TaggedTerminal(..))
import qualified Data.Automaton.DVA as DVA

import ErrorMessage (FormatError(..), simpleErrorMessage)

import P1Lexing.Types (Range, textualRange, Ranged)
import P2LanguageDefinition.Types (TypeName, Name(..))
import P4Parsing.Types (NodeF(..), allNodeFChildren)
import P5DynamicAmbiguity.TreeLanguage (treeLanguage, PreLanguage)
import P5DynamicAmbiguity.Types

type AmbTree elidable tok = Either (Either (tok, tok) (NodeF tok (tok, tok))) (NodeOrElide elidable tok)

data Error elidable tok = Ambiguity
  !Range
  !TimeoutInfo
  ![(AmbTree elidable tok, (Text, Bool))]  -- ^ Resolvable alternatives
  ![AmbTree elidable tok]  -- ^ Unresolvable alternatives
data AmbiguityStyle = Resolvable | Unresolvable | Mixed
ambiguityStyle :: Error elidable tok -> AmbiguityStyle
ambiguityStyle (Ambiguity _ _ (_:_) (_:_)) = Mixed
ambiguityStyle (Ambiguity _ _ [] (_:_)) = Unresolvable
ambiguityStyle (Ambiguity _ _ (_:_) []) = Resolvable
ambiguityStyle (Ambiguity _ _ [] []) = panic $ "Unexpectedly empty ambiguity error"

didTimeout :: Error elidable tok -> Bool
didTimeout (Ambiguity _ DidTimeout _ _) = True
didTimeout _ = False

countReparseFailures :: Error elidable tok -> Int
countReparseFailures (Ambiguity _ _ resolvable _) =
  resolvable
  & filter (snd >>> snd >>> not)
  & length

forceResolutions :: Error elidable tok -> Error elidable tok
forceResolutions e@(Ambiguity _ _ resolvable _) = concatted `seq` e
  where
    concatted = foldMap (snd >>> fst) resolvable

instance Eq tok => FormatError (Error elidable tok) where
  type ErrorOpts (Error elidable tok) = ErrorOptions elidable tok
  formatError EO{showTwoLevel, elidedRange, showElided, showTok, tokRange} (Ambiguity r timeoutInfo resolvable trees) = simpleErrorMessage r $
    kind <> " with " <> show (length trees + length resolvable) <> " alternatives.\n" <>
    reparseInfo <>
    resolvableSection <>
    unresolvableSection
    where
      hasResolvable = not $ null resolvable
      hasUnresolvable = not $ null trees
      hasReparseFailures = any (snd >>> snd >>> not) resolvable
      reparseInfo | hasReparseFailures = "Resolutions marked '<(!)>' cause another ambiguity error\n that is no smaller than this error.\n"
                  | otherwise = ""
      timeoutStr | DidTimeout <- timeoutInfo = " (timeout)"
                 | otherwise = ""
      kind | hasUnresolvable = "Unresolvable ambiguity error" <> timeoutStr
           | otherwise = "Ambiguity error"
      resolvableSection
        | not hasResolvable = ""
        | hasUnresolvable = "\nResolvable alternatives:\n" <> formattedResolvable
        | otherwise = "\n" <> formattedResolvable
      formattedT (t, reparses)
        | hasReparseFailures = "  " <> (if reparses then "      " else "<(!)> ") <> t <> "\n"
        | otherwise = "  " <> t <> "\n"
      formattedResolvable
        | showTwoLevel = resolvable
          <&> (twoLevel *** S.singleton)
          & M.fromListWith S.union
          & M.toList
          & foldMap (\(two, ts) -> foldMap formattedT ts <> two <> "\n")
        | otherwise = resolvable
          & foldMap (snd >>> formattedT)
      unresolvableSection
        | not hasUnresolvable = ""
        | hasResolvable = "\nUnresolvable alternatives:\n" <> formattedUnresolvable
        | otherwise = "\n" <> formattedUnresolvable
      formattedUnresolvable = trees <&> twoLevel & S.fromList & fold
      showTokRange (t1, t2)
        | t1 == t2 = showTok t1
        | otherwise = "..."
      tokRangeRange (t1, t2) = tokRange t1 <> tokRange t2
      twoLevel :: AmbTree elidable tok -> Text
      twoLevel (Right (Elide elided)) = "  " <> showElided elided <> "\n"
      twoLevel (Right (Node n@NodeF{n_nameF})) = "  " <> coerce n_nameF <> formatChildren (allNodeFChildren n) <> "\n"
        where
          range (Node NodeF{n_rangeF}) = n_rangeF
          range (Elide elided) = elidedRange elided
          formatChildren = sortBy (comparing $ either tokRange range) >>> foldMap formatNode
          formatNode (Right (Elide elided)) =
            printf "\n   - %- 20s %s" (showElided elided) (textualRange $ elidedRange elided) & Text.pack
          formatNode (Right (Node NodeF{n_nameF = Name name, n_rangeF})) =
            printf "\n   - %- 20s %s" name (textualRange n_rangeF) & Text.pack
          formatNode (Left t) =
            printf "\n   - %- 20s %s" (showTok t) (textualRange (tokRange t)) & Text.pack
      twoLevel (Left (Left tokPair)) = "  " <> showTokRange tokPair <> "\n"
      twoLevel (Left (Right n@NodeF{n_nameF})) = "  " <> coerce n_nameF <> formatChildren (allNodeFChildren n) <> "\n"
        where
          formatChildren = sortBy (comparing $ either tokRange tokRangeRange) >>> foldMap formatNode
          formatNode (Right tokPair) =
            printf "\n   - %- 20s %s" (showTokRange tokPair) (textualRange $ tokRangeRange tokPair) & Text.pack
          formatNode (Left t) =
            printf "\n   - %- 20s %s" (showTok t) (textualRange (tokRange t)) & Text.pack

data ErrorOptions elidable tok = EO
  { showTwoLevel :: Bool
  , showElided :: elidable -> Text
  , elidedRange :: elidable -> Range
  , showTok :: tok -> Text
  , tokRange :: tok -> Range
  }

data DynAnalysisKind = FastDyn | CompleteDyn | RaceDyn

data DynConfig elidable tok = DynConfig
  { dTimeout :: Int  -- ^ Timeout in microseconds. Negative to never timeout. Only honored by FastDyn.
  , dPl :: PreLanguage elidable
  , dMkToken :: tok -> Token elidable
  , dGetElided :: elidable -> (Range, TypeName)
  , dGetElidedTokRange :: elidable -> (tok, tok)
  , dShowElided :: elidable -> Text
  , dCheckReparses :: [Token elidable] -> Bool
  , dKind :: DynAnalysisKind
  , dGroupByTop :: Bool
  }

type ResLang elidable = NVA Int Int (Token elidable) (Token elidable) (Token elidable)
type ResStr elidable = [TaggedTerminal (Token elidable) (Token elidable) (Token elidable)]

analyze :: forall elidable tok. (Eq elidable, Hashable elidable, Show elidable, Show tok, Ranged tok, Eq tok, Hashable tok, NFData tok, NFData elidable)
        => DynConfig elidable tok
        -> HashSet (NodeOrElide elidable tok)
        -> IO (Error elidable tok)
analyze config@DynConfig{dPl, dGetElided, dMkToken, dKind, dCheckReparses, dShowElided, dGroupByTop, dGetElidedTokRange} alts =
  (if dGroupByTop then (first Left <$> groupedLanguages) else (first Right <$> languages))
  & analyze'
  <&> second prepareForAmbiguity
  <&> \(timeoutInfo, (res, unres)) -> Ambiguity range timeoutInfo res unres
  where
    range = toList alts & \case
      Node NodeF{n_rangeF} : _ -> n_rangeF
      Elide elidable : _ -> dGetElided elidable & fst
      [] -> compErr "P5DynamicAmbiguity.analyze.range" "got zero alternatives"
    mkLanguage = treeLanguage dPl dMkToken dGetElided

    languages :: [(NodeOrElide elidable tok, ResLang elidable)]
    languages = toList alts
      <&> (identity &&& mkLanguage)

    groupedLanguages :: [(Either (tok, tok) (NodeF tok (tok, tok)), ResLang elidable)]
    groupedLanguages = languages
      <&> (extractTop *** Seq.singleton)
      & M.fromListWith (<>)
      <&> unions
      & M.toList

    mkErrorEntry :: forall tree. (tree, Maybe (ResStr elidable)) -> Either (tree, (Text, Bool)) tree
    mkErrorEntry (t, Nothing) = Right t
    mkErrorEntry (t, Just w) =
      let stream = untag identity identity identity <$> w
      in textualToken dShowElided <$> stream
         & Text.unwords
         & (, dCheckReparses stream)
         & (t,)
         & Left

    prepareForAmbiguity :: forall tree. [(tree, Maybe (ResStr elidable))]
                        -> ([(tree, (Text, Bool))], [tree])
    prepareForAmbiguity = fmap mkErrorEntry >>> partitionEithers

    extractTop :: NodeOrElide elidable tok -> Either (tok, tok) (NodeF tok (tok, tok))
    extractTop (Elide e) = dGetElidedTokRange e & Left
    extractTop (Node n) = n <&> mkTokRange & Right
    mkTokRange :: NodeOrElide elidable tok -> (tok, tok)
    mkTokRange (Elide e) = dGetElidedTokRange e
    mkTokRange (Node n) = n_beginEndF n
      & compFromJust "P5DynamicAmbiguity.Analysis.analyze.mkTokRange" "Missing beginEnd on NodeF"

    analyze' :: forall tree. NFData tree => [(tree, ResLang elidable)] -> IO (TimeoutInfo, [(tree, Maybe (ResStr elidable))])
    analyze' = case dKind of
      FastDyn -> fastAnalyze config
      CompleteDyn -> completeAnalyze config
      RaceDyn -> \tree -> race (fastAnalyze config tree) (completeAnalyze config tree)
        <&> either identity identity

fastAnalyze :: forall tree elidable tok. (Eq elidable, Hashable elidable)
            => DynConfig elidable tok -> [(tree, ResLang elidable)]
            -> IO (TimeoutInfo, [(tree, Maybe (ResStr elidable))])
fastAnalyze DynConfig{dTimeout} alts = do
  (timeoutInfo, res) <- shortestUniqueWord dTimeout (longestShortestAmbiguous + 10) nvas
  return (timeoutInfo, zip trees (snd <$> res))
  where
    (trees, nvas) = unzip alts

    Max longestShortestAmbiguous = foldMap (shortestWord >>> fmap (length >>> Max) >>> fold) nvas

completeAnalyze :: forall tree elidable tok. (Eq elidable, Hashable elidable, NFData tree, NFData elidable)
                => DynConfig elidable tok -> [(tree, ResLang elidable)]
                -> IO (TimeoutInfo, [(tree, Maybe (ResStr elidable))])
completeAnalyze DynConfig{} alts = alts
  & oneAndOthers
  <&> second (fmap snd)
  <&> findShortest
  & force
  & evaluate
  <&> (DidNotTimeout,)
  where
    oneAndOthers :: [a] -> [(a, [a])]
    oneAndOthers [] = []
    oneAndOthers (a : as) = (a, as) : (second (a:) <$> oneAndOthers as)

    findShortest ((t, this), others) = unions others
      & DVA.determinize
      & DVA.difference (DVA.determinize this)
      & DVA.asNVA
      & reduce
      & shortestWord
      & (t,)
