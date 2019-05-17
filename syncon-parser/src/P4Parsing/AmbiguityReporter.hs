{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module P4Parsing.AmbiguityReporter
( report
, Error(..)
) where

import Pre hiding (reduce)
import Result (Result(..))

import Text.Printf (printf)
import qualified Data.Text as Text
import qualified Data.HashSet as S
import qualified Data.HashMap.Lazy as M
import qualified Data.Sequence as Seq

import Data.Generics.Uniplate.Data (universe)
import Data.Functor.Foldable (project)

import Data.Automaton.NVA (fromEpsNVA, shortestWord, trim)
import Data.Automaton.EpsilonNVA (EpsNVA(..), untag, fromNFA, renumberStack, TaggedTerminal(..))
import qualified Data.Automaton.EpsilonNVA as EpsNVA
import Data.Automaton.DVA (DVA(..), determinize, renumber, difference, asNVA)
import Data.Automaton.Regex (Regex(..))
import qualified Data.Automaton.Regex as Regex

import ErrorMessage (FormatError(..), simpleErrorMessage)

import P1Lexing.Types (Range, range, textualRange)
import qualified P1Lexing.Types as P1
import P2LanguageDefinition.Types (Name(..), SDName(..), TypeName(..), Syncon(..), SyntaxDescription(..), BracketKind(..), DefinitionFile(..))
import P2LanguageDefinition.Elaborator (elaborate)
import P4Parsing.Types (pattern Node, n_name, n_contents, NodeInternals(..))
import qualified P4Parsing.Types as P4
import P4Parsing.Parser (SingleLanguage(..))

data Error
  = Ambiguity Range [Text]
  | Unresolvable Range [Text] [Node]  -- ^ Resolvable alternatives, and unresolvable alternatives
  deriving (Show)

type Node = P4.Node SingleLanguage TypeName

instance FormatError Error where
  formatError (Unresolvable r resolvable trees) = simpleErrorMessage r $
    "Unresolvable ambiguity error with " <> show (length trees + length resolvable) <> " alternatives.\n" <>
    formattedResolvable <> "\n" <>
    foldMap ((n_name &&& (project >>> toList)) >>> format) trees
    where
      format (name, children) = "  " <> coerce name <> formatChildren children <> "\n"
      formatChildren = sortBy (compare `on` range) >>> foldMap formatNode
      formatNode n@Node{n_name = Name n'} =
        printf "\n   - %- 20s %s" n' (textualRange $ range n) & Text.pack
      formattedResolvable
        | null resolvable = ""
        | otherwise = "\nResolvable alternatives:\n" <> foldMap (\t -> "  " <> t <> "\n") resolvable
  formatError (Ambiguity r resolvable) = simpleErrorMessage r $
    "Ambiguity error with " <> show (length resolvable) <> " alternatives:\n" <>
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

type ResLang = DVA Int Int Token Token Token

resolvability :: DefinitionFile -> Range -> HashSet Node -> Error
resolvability df r nodes = M.toList unambigLanguages
  <&> (\(node, lang) ->
         case asNVA lang & trim & shortestWord of
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
    info :: HashMap Name (SyntaxDescription, HashMap (SavedPoint ()) Path, HashSet Path)
    info = syncons df <&> \Syncon{..} ->
      let sd = s_syntaxDescription
          sps = discoverSavePoints (snd s_syntaxType) sd
          paths = S.fromList $ toList sps
      in (sd, sps, paths)

    elaboration = elaborate (syncons df) (forbids df) (precedences df)
    elaborationWithRecs :: HashMap (Name, SDName) (HashSet Name)
    elaborationWithRecs = M.unionWith S.union fixedOriginals recAdditions
      where
        fixedOriginals = M.toList elaboration
          & mapMaybe (\case
                         ((n1, Right sdname), names) -> Just ((n1, sdname), names)
                         _ -> Nothing)
          & M.fromList
        recAdditions = M.fromListWith S.union $ do
          (n, syncon) <- M.toList $ syncons df
          universe (s_syntaxDescription syncon) >>= \case
            SDNamed _ sdname (SDRec _ sdrec) ->
              pure ((n, sdname), M.lookupDefault S.empty (n, Left sdrec) elaboration)
            _ -> []

    isForbidden :: Name -> SDName -> Name -> Bool
    isForbidden n1 sdname n2 = M.lookup (n1, sdname) elaborationWithRecs
      & fold
      & S.member n2

    tokGroupings :: HashMap TypeName (Seq (Token, Token))
    tokGroupings = groupings df <&> fmap (mkTok *** mkTok)
    getGrouping :: TypeName -> Seq (Token, Token)
    getGrouping tyn = M.lookup tyn tokGroupings & fold

    languages :: HashMap Node ResLang
    languages = S.toMap nodes
      & M.mapWithKey (\node _ -> mkLanguage isForbidden (bracketKind df) getSyTy getGrouping info node
                       & fromEpsNVA
                       & determinize
                       & renumber)

    unambigLanguages :: HashMap Node ResLang
    unambigLanguages = M.mapWithKey mkUnambig languages
    mkUnambig node lang = S.delete node nodes
      & toList
      & fmap (\n -> M.lookup n languages
               & compFromJust "P4Parsing.AmbiguityReporter.resolvability.mkUnambig" "Missed computing a language for a node")
      & foldl' langDiff lang

    langDiff :: ResLang -> ResLang -> ResLang
    langDiff a b = difference a b & asNVA & trim & determinize & renumber

    getSyTy :: Name -> TypeName
    getSyTy n = M.lookup n (syncons df)
      & compFromJust "P4Parsing.AmbiguityReporter.nodePoints.getSyTy" "Syncon without a type"
      & s_syntaxType
      & snd

data Token
  = LitTok Text
  | OtherTokInstance TypeName Text
  | OtherTok TypeName
  deriving (Show)

eitherRepr :: Token -> Either Text TypeName
eitherRepr (LitTok t) = Left t
eitherRepr (OtherTokInstance n _) = Right n
eitherRepr (OtherTok n) = Right n

textualToken :: Token -> Text
textualToken (LitTok t) = t
textualToken (OtherTokInstance _ t) = t
textualToken (OtherTok tyn) = coerce tyn  -- TODO: make this more distinct somehow

mkTok :: Either Text TypeName -> Token
mkTok (Left t) = LitTok t
mkTok (Right n) = OtherTok n

instance Eq Token where
  (==) = (==) `on` eitherRepr
instance Hashable Token where
  hashWithSalt = hashUsing eitherRepr

-- | Generate a 'NVA' that recognizes the words that can parse as a given AST.
-- The NVA will push and pop 'Nothing' for opening and closing brackets internal
-- to a syncon, and Just n for the nth paren location, to ensure that grouping
-- parens are paired appropriately. (TODO: update documentation comment)
mkLanguage :: (Name -> SDName -> Name -> Bool)  -- ^ True if forbidden
           -> (Either Text TypeName -> BracketKind)
           -> (Name -> TypeName)
           -> (TypeName -> Seq (Token, Token))
           -> HashMap Name ( SyntaxDescription
                           , HashMap (SavedPoint ()) Path
                           , HashSet Path )
           -> Node
           -> EpsNVA Int Int Token Token Token
mkLanguage isForbidden bracketKind getSyTy getGroupings syncons n@Node{n_name} =
  zipWith genSegment (toList points) (tail $ fst <$> toList points)
  & mconcat
  where
    groupings = getGroupings (getSyTy n_name)
      & toList
      & zip [-1,-2..]
      & Seq.fromList
    (sd, pointToPathMap, paths) = M.lookup n_name syncons
      & compFromJust "P4Parsing.AmbiguityReporter.mkLanguage" "Missing syncon"
    pointToPath point = M.lookup (void point) pointToPathMap
      & compFromJust "P4Parsing.AmbiguityReporter.mkLanguage" "Missing path for point"

    points = (Start, Nothing) Seq.:<| (((pointToPath &&& Just) <$> nodePoints getSyTy n) Seq.:|> (End, Nothing))

    genSegment :: (Path, Maybe (SavedPoint Node))
               -> Path
               -> EpsNVA Int Int Token Token Token
    genSegment (start, point) end =
      foldMap genPoint point <> regexToEpsNVA (regexIntermission sd start end paths)

    genPoint :: SavedPoint Node -> EpsNVA Int Int Token Token Token
    genPoint (TokPoint _ tok) = regexToEpsNVA $ Terminal tok
    genPoint (NodePoint sdname _ node@Node{n_name=innerName})
      | isForbidden n_name sdname innerName = withOpt
        { EpsNVA.initial = S.singleton (-1)
        , EpsNVA.final = S.singleton (-2)
        , EpsNVA.openTransitions = mkMandEdge (-1) <$> toList (EpsNVA.initial withOpt) <*> (second fst <$> toList groupings)
          & EpsNVA.fromTriples
          & addTransitions (EpsNVA.openTransitions withOpt)
        , EpsNVA.closeTransitions = mkMandEdge <$> toList (EpsNVA.final withOpt) <*> pure (-2) <*> (second snd <$> toList groupings)
          & EpsNVA.fromTriples
          & addTransitions (EpsNVA.closeTransitions withOpt) }
      | otherwise = withOpt
      where
        withOpt = innerLang
          { EpsNVA.openTransitions = addTransitions (EpsNVA.openTransitions innerLang) optOpens
          , EpsNVA.closeTransitions = addTransitions (EpsNVA.closeTransitions innerLang) optCloses }
        optOpens = mkOptEdge <$> toList (EpsNVA.initial innerLang) <*> (second fst <$> toList groupings)
          & EpsNVA.fromTriples
        optCloses = mkOptEdge <$> toList (EpsNVA.final innerLang) <*> (second snd <$> toList groupings)
          & EpsNVA.fromTriples
        mkOptEdge s (sta, o) = (s, o, (sta, s))
        mkMandEdge s1 s2 (sta, o) = (s1, o, (sta, s2))
        addTransitions = M.unionWith $ M.unionWith S.union
        innerLang = mkLanguage isForbidden bracketKind getSyTy getGroupings syncons node

    regexToEpsNVA :: Regex Token -> EpsNVA Int Int Token Token Token
    regexToEpsNVA = Regex.toAutomaton >>> fromNFA tag >>> renumberStack

    tag :: Token -> TaggedTerminal Token Token Token
    tag tok = case bracketKind $ eitherRepr tok of
      OpenBracket -> Open tok
      NonBracket -> Inner tok
      CloseBracket -> Close tok

    tail :: [a] -> [a]
    tail [] = []
    tail (_:as) = as

-- | A 'SavedPoint' is enough to specify a singular position in the syntax description,
-- even in the presence of name:(a | b | c).
-- TODO: make sure the BasicChecker enforces this (i.e., alt of non-seqs have distinct alts)
-- TODO: make SDAlt parse as either an alt of seqs, or an alt of non-seqs, never a mix
data SavedPoint node
  = TokPoint SDName Token
  | NodePoint SDName TypeName node
  deriving (Functor, Eq, Generic)
instance Hashable node => Hashable (SavedPoint node)

-- | Produce a list of the saved points of a single node, in the order they appeared in
-- the original source.
nodePoints :: (Name -> TypeName) -> Node -> Seq (SavedPoint Node)
nodePoints getSyTy = n_contents >>> mapRecur >>> Seq.sortBy (comparing fst) >>> fmap snd
  where
    mapRecur = M.toList >>> foldMap (\(name, children) -> children >>= recur name)
    recur name (NodeLeaf n@Node{n_name}) = Seq.singleton
      (range n, NodePoint name (getSyTy n_name) n)
    recur name (TokenLeaf (P1.LitTok r _ t)) = Seq.singleton $
      (r, TokPoint name (LitTok t))
    recur name (TokenLeaf (P1.OtherTok r _ n t)) = Seq.singleton $
      (r, TokPoint name (OtherTokInstance n t))
    recur _ (Struct contents) = mapRecur contents

-- | A 'Path' describes where a 'SavedPoint' is in a syntax description. Each 'Int'
-- selects the corresponding index in an 'SDSeq' or 'SDAlt'. Each single child
-- syntax description is stepped through implicitly. There are also two special paths
-- that represent the beginning and end of the syntax description, respectively.
data Path
  = Start
  | Path (Seq Int)
  | End
  deriving (Eq, Ord, Generic)
instance Hashable Path where
  hashWithSalt s Start = s `hashWithSalt` (0::Int)
  hashWithSalt s (Path path) = s `hashWithSalt` (1::Int) `hashWithSalt` toList path
  hashWithSalt s End = s `hashWithSalt` (2::Int)

discoverSavePoints :: TypeName -> SyntaxDescription -> HashMap (SavedPoint ()) Path
discoverSavePoints selftyn = recur Nothing []
  where
    recur :: Maybe SDName -> [Int] -> SyntaxDescription -> HashMap (SavedPoint ()) Path
    recur _ path (SDSeq _ sds) =  [0..] `zip` toList sds
      & foldMap (\(idx, sd) -> recur Nothing (idx:path) sd)
    recur name path (SDAlt _ sds) = [0..] `zip` toList sds
      & foldMap (\(idx, sd) -> recur name (idx:path) sd)
    recur _ path (SDRep _ _ sd) = recur Nothing path sd
    recur _ path (SDNamed _ name sd) = recur (Just name) path sd
    recur (Just name) path (SDSyTy _ tyn) = M.singleton (NodePoint name tyn ()) (Path $ Seq.fromList path)
    recur (Just name) path (SDRec _ _) = M.singleton (NodePoint name selftyn ()) (Path $ Seq.fromList path)
    recur (Just name) path (SDToken _ t) = M.singleton (TokPoint name (LitTok t)) (Path $ Seq.fromList path)
    recur _ _ _ = M.empty

regexIntermission :: SyntaxDescription -> Path -> Path -> HashSet Path -> Regex Token -- TODO: make something real here
regexIntermission _sd start end (S.delete start . S.delete end -> _others) = Eps
