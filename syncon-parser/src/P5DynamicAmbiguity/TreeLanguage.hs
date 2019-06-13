{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module P5DynamicAmbiguity.TreeLanguage (precompute, PreLanguage, treeLanguage, fastShortest) where

import Pre hiding (reduce)

import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import qualified Data.Sequence as Seq
import Data.Sequence (pattern Empty, pattern (:<|), pattern (:|>))

import Data.Generics.Uniplate.Data (universe)

import Data.Automaton.EpsilonNVA (EpsNVA(..), fromNFA, TaggedTerminal(..))
import qualified Data.Automaton.EpsilonNVA as EpsNVA
import Data.Automaton.NVA (fromEpsNVA, NVA, reduce, renumber)
import Data.Automaton.Regex (Regex(..))
import qualified Data.Automaton.Regex as Regex

import P1Lexing.Types (range)
import qualified P1Lexing.Types as P1
import P2LanguageDefinition.Elaborator (elaborate)
import P2LanguageDefinition.Types (Name(..), SDName(..), TypeName(..), Syncon(..), SyntaxDescription(..), BracketKind(..), DefinitionFile, Repetition(..))
import qualified P2LanguageDefinition.Types as P2
import P4Parsing.Parser (SingleLanguage(..))
import P4Parsing.Types (pattern Node, n_name, n_contents, NodeInternals(..))
import qualified P4Parsing.Types as P4
import P5DynamicAmbiguity.Types

type Node = P4.Node SingleLanguage TypeName

-- | Take a 'PreLanguage' and a 'Node' computed/parsed using the same language definition, then
-- produce a reduced NVA recognizing the language of words that can be parsed as that
-- node.
--
-- Note that the 'Node' doesn't have to be of syntax type Top, it can be any syntax type.
treeLanguage :: PreLanguage -> Node -> NVA Int Int Token Token Token
treeLanguage pl =
  mkLanguage pl
  >>> fromEpsNVA
  >>> reduce
  >>> renumber

data PreLanguage = PreLanguage
  { isForbidden :: Name -> SDName -> Name -> Bool
  , bracketKind :: Either Text TypeName -> BracketKind
  , getSyTy :: Name -> TypeName
  , getGroupings :: TypeName -> Seq (Token, Token)
  , syncons :: HashMap Name ( SyntaxDescription
                            , HashMap (SavedPoint ()) Path
                            , HashSet Path )
  }

-- | Do some precomputations that are useful for the remaining functions in this module, to
-- enable sharing some work.
precompute :: DefinitionFile -> PreLanguage
precompute df = PreLanguage{..}
  where
    elaboration = elaborate (P2.syncons df) (P2.forbids df) (P2.precedences df)
    elaborationWithRecs :: HashMap (Name, SDName) (HashSet Name)
    elaborationWithRecs = M.unionWith S.union fixedOriginals recAdditions
      where
        fixedOriginals = M.toList elaboration
          & mapMaybe (\case
                         ((n1, Right sdname), names) -> Just ((n1, sdname), names)
                         _ -> Nothing)
          & M.fromList
        recAdditions = M.fromListWith S.union $ do
          (n, syncon) <- M.toList $ P2.syncons df
          universe (s_syntaxDescription syncon) >>= \case
            SDNamed _ sdname (SDRec _ sdrec) ->
              pure ((n, sdname), M.lookupDefault S.empty (n, Left sdrec) elaboration)
            _ -> []

    bracketKind = P2.bracketKind df

    isForbidden :: Name -> SDName -> Name -> Bool
    isForbidden n1 sdname n2 = M.lookup (n1, sdname) elaborationWithRecs
      & fold
      & S.member n2

    getSyTy :: Name -> TypeName
    getSyTy n = M.lookup n (P2.syncons df)
      & compFromJust "P4Parsing.AmbiguityReporter.nodePoints.getSyTy" "Syncon without a type"
      & s_syntaxType
      & snd

    tokGroupings :: HashMap TypeName (Seq (Token, Token))
    tokGroupings = P2.groupings df <&> fmap (mkTok *** mkTok)
    getGroupings :: TypeName -> Seq (Token, Token)
    getGroupings tyn = M.lookup tyn tokGroupings & fold

    syncons :: HashMap Name (SyntaxDescription, HashMap (SavedPoint ()) Path, HashSet Path)
    syncons = P2.syncons df <&> \Syncon{..} ->
      let sd = s_syntaxDescription
          sps = discoverSavePoints isSyTy (snd s_syntaxType) sd
          paths = S.fromList $ toList sps
      in (sd, sps, paths)

    isSyTy :: TypeName -> Bool
    isSyTy tyn = M.lookup tyn (P2.syntaxTypes df) & maybe False isLeft

-- | Produce the shortest sequence of 'Token's that could be parsed as the given
-- node. Note that there is no guarantee at all that the produced sequence is
-- unambiguous, merely that it is valid.
fastShortest :: PreLanguage -> Node -> Seq Token
fastShortest pl@PreLanguage{..} n@Node{n_name} =
  zipWith genSegment (toList points) (tail $ fst <$> toList points)
  & mconcat
  where
    (sd, _, paths) = M.lookup n_name syncons
      & compFromJust "P4Parsing.AmbiguityReporter.mkLanguage" "Missing syncon"
    groupings = getGroupings $ getSyTy n_name

    points = pathPoints pl n

    genSegment :: (Path, Maybe (SavedPoint Node))
               -> Path
               -> Seq Token
    genSegment (start, point) end =
      foldMap genPoint point <> Regex.shortestWord (regexIntermission sd start end paths)

    genPoint :: SavedPoint Node -> Seq Token
    genPoint (TokPoint _ tok) = Seq.singleton tok
    genPoint (NodePoint sdname _ node@Node{n_name=innerName})
      | isForbidden n_name sdname innerName = groupings
        & head
        & compFromJust "P5DynamicAmbiguity.TreeLanguage.fastShortest.genPoint"
          ("No grouping available for syncon " <> coerce innerName <> ", but it's required since it's forbidden inside " <> coerce n_name)
        & \(pre, post) -> pre Seq.:<| (result Seq.:|> post)
      | otherwise = result
      where
        result = fastShortest pl node

    tail :: [a] -> [a]
    tail [] = []
    tail (_:as) = as

-- | Generate a 'NVA' that recognizes the words that can parse as a given AST.
-- The NVA will push and pop 'Nothing' for opening and closing brackets internal
-- to a syncon, and Just n for the nth paren location, to ensure that grouping
-- parens are paired appropriately. (TODO: update documentation comment)
mkLanguage :: PreLanguage
           -> Node
           -> EpsNVA Int (Either () Int) Token Token Token
mkLanguage pl@PreLanguage{..} n@Node{n_name} =
  zipWith genSegment (toList points) (tail $ fst <$> toList points)
  & mconcat
  where
    (sd, _, paths) = M.lookup n_name syncons
      & compFromJust "P4Parsing.AmbiguityReporter.mkLanguage" "Missing syncon"

    points = pathPoints pl n

    genSegment :: (Path, Maybe (SavedPoint Node))
               -> Path
               -> EpsNVA Int (Either () Int) Token Token Token
    genSegment (start, point) end =
      foldMap genPoint point <> regexToEpsNVA (regexIntermission sd start end paths)

    genPoint :: SavedPoint Node -> EpsNVA Int (Either () Int) Token Token Token
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
        syTy = getSyTy innerName
        groupings = getGroupings syTy
          & toList
          & zip [-1,-2..]
          <&> first Right
          & Seq.fromList
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
        innerLang = mkLanguage pl node

    regexToEpsNVA :: Regex Token -> EpsNVA Int (Either () Int) Token Token Token
    regexToEpsNVA = Regex.toAutomaton >>> fromNFA tag >>> EpsNVA.mapSta Left

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
  deriving (Functor, Eq, Generic, Show)
instance Hashable node => Hashable (SavedPoint node)

asLookupPoint :: SavedPoint node -> SavedPoint ()
asLookupPoint (TokPoint sdname (OtherTokInstance tyn _)) = TokPoint sdname (OtherTok tyn)
asLookupPoint (TokPoint sdname tok) = TokPoint sdname tok
asLookupPoint (NodePoint sdname tyn _) = NodePoint sdname tyn ()

-- | Produce a list of paths of a given 'Node'. This is essentially the result of
-- 'nodePoints', but converted to 'Path's, plus a 'Start' and 'End'. The original
-- node points remain as the second component of the tuple, i.e., the second component
-- is 'Nothing' iff the first component is 'Start' or 'End'.
pathPoints :: PreLanguage -> Node -> Seq (Path, Maybe (SavedPoint Node))
pathPoints PreLanguage{syncons,getSyTy} n@Node{n_name} = (Start, Nothing) Seq.:<|
  (((pointToPath &&& Just) <$> nodePoints getSyTy n)
   Seq.:|> (End, Nothing))
  where
    (_, pointToPathMap, _) = M.lookup n_name syncons
      & compFromJust "P4Parsing.AmbiguityReporter.findPoints" "Missing syncon"
    pointToPath point = M.lookup (asLookupPoint point) pointToPathMap
      & compFromJust "P4Parsing.AmbiguityReporter.findPoints" ("Missing path for point " <> show point <> " in " <> show pointToPathMap)

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
  deriving (Eq, Ord, Generic, Show)
instance Hashable Path where
  hashWithSalt s Start = s `hashWithSalt` (0::Int)
  hashWithSalt s (Path path) = s `hashWithSalt` (1::Int) `hashWithSalt` toList path
  hashWithSalt s End = s `hashWithSalt` (2::Int)

discoverSavePoints :: (TypeName -> Bool) -> TypeName -> SyntaxDescription -> HashMap (SavedPoint ()) Path
discoverSavePoints isSyTy selftyn = recur Nothing Empty
  where
    recur :: Maybe SDName -> Seq Int -> SyntaxDescription -> HashMap (SavedPoint ()) Path
    recur _ path (SDSeq _ sds) =  [0..] `zip` toList sds
      & foldMap (\(idx, sd) -> recur Nothing (path :|> idx) sd)
    recur name path (SDAlt _ sds) = [0..] `zip` toList sds
      & foldMap (\(idx, sd) -> recur name (path :|> idx) sd)
    recur _ path (SDRep _ _ sd) = recur Nothing path sd
    recur _ path (SDNamed _ name sd) = recur (Just name) path sd
    recur (Just name) path (SDSyTy _ tyn)
      | isSyTy tyn = M.singleton (NodePoint name tyn ()) (Path path)
      | otherwise = M.singleton (TokPoint name (OtherTok tyn)) (Path path)
    recur (Just name) path (SDRec _ _) = M.singleton (NodePoint name selftyn ()) (Path path)
    recur (Just name) path (SDToken _ t) = M.singleton (TokPoint name (LitTok t)) (Path path)
    recur _ _ _ = M.empty

data IntermissionResult
  = Done (Regex Token)
  | Partial (Regex Token)
  | Fail

instance Semigroup IntermissionResult where
  a@Done{} <> _ = a
  a@Fail <> _ = a
  _ <> b@Fail = b
  Partial a <> Partial b = Partial $ a <> b
  Partial a <> Done b = Done $ a <> b
instance Monoid IntermissionResult where
  mappend = (<>)
  mempty = Partial Eps

-- | Extract the part between two 'SavedPoint's, creating a regex for what should be there
regexIntermission :: SyntaxDescription -> Path -> Path -> HashSet Path -> Regex Token
regexIntermission fullSd start end (S.delete end -> others) = case (recur fullSd Empty start, end) of
  (Done reg, _) -> reg
  (Partial reg, End) -> reg
  (Partial reg, _) -> compErr "P4Parsing.AmbiguityReporter.RegexIntermission" $
    "Could not construct a regex intermission between " <> show start <> " and " <> show end <> "\n"
    <> "but got a partial: " <> show reg
  (Fail, _) -> compErr "P4Parsing.AmbiguityReporter.RegexIntermission" $
    "Could not construct a regex intermission between " <> show start <> " and " <> show end
  where
    -- | sd -> current (i.e., current prefix, the path taken to get to here) -> target (without prefix) -> result
    recur :: SyntaxDescription -> Seq Int -> Path -> IntermissionResult

    -- Leaves
    recur (SDToken _ _) _ (Path Empty) = Partial Eps
    recur (SDToken _ t) path Start
      | Path path `S.member` others = Fail
      | Path path == end = Done Eps
      | otherwise = Partial $ Terminal $ mkTok $ Left t
    recur (SDSyTy _ _) _ (Path Empty) = Partial Eps
    recur (SDSyTy _ tyn) path Start
      | Path path `S.member` others = Fail
      | Path path == end = Done Eps
      | otherwise = Partial $ Terminal $ mkTok $ Right tyn
    recur (SDRec _ _) _ (Path Empty) = Partial Eps
    recur (SDRec _ _) path Start
      | Path path `S.member` others = Fail
      | Path path == end = Done Eps

    -- Wide nodes
    recur (SDSeq _ sds) path target
      | Path (idx :<| contPath) <- target
      , sd : rest <- drop idx $ toList sds
      = recur sd (path :|> idx) (Path contPath) <> foldMap restf ([idx+1..] `zip` rest)
      | Start <- target = foldMap restf ([0..] `zip` toList sds)
      where
        restf (idx, sd) = recur sd (path :|> idx) Start
    recur (SDAlt _ sds) path (Path (idx :<| contPath))
      | Just sd <- Seq.lookup idx sds = recur sd (path :|> idx) (Path contPath)
    recur (SDAlt _ sds) path Start
      | Just res <- find isDone results = res
      | not $ null partialResults = partialResults & Regex.choice & Partial
      | otherwise = Fail
      where
        results = [0..] `zip` toList sds
          <&> (\(idx, sd) -> recur sd (path :|> idx) Start)
          & Seq.fromList
        partialResults = toList results & mapMaybe fromPartial

    -- Single child nodes
    recur (SDNamed _ _ sd) path target = recur sd path target
    recur (SDRep _ RepStar sd) path target@Path{} =
      recur sd path target <> asNonFail (recur sd path Start)
    recur (SDRep _ RepStar sd) path Start = case recur sd path Start of
      Fail -> Partial Eps
      Partial reg -> Partial $ Kleene reg
      Done reg -> Done reg
    recur (SDRep _ RepPlus sd) path target@Path{} =
      recur sd path target <> asNonFail (recur sd path Start)
    recur (SDRep _ RepPlus sd) path Start =
      result <> mapResult Kleene result
      where
        result = recur sd path Start
    recur (SDRep _ RepQuestion sd) path target@Path{} = recur sd path target
    recur (SDRep _ RepQuestion sd) path Start = recur sd path Start
      & mapPartial (Choice Eps)
      & asNonFail

    recur sd path target = compErr "P4Parsing.AmbiguityReporter.RegexIntermission.recur" $
      "Bad case: sd: " <> show sd <> ", path: " <> show path <> ", target: " <> show target

    asNonFail Fail = mempty
    asNonFail a = a
    isDone Done{} = True
    isDone _ = False
    fromPartial (Partial a) = Just a
    fromPartial _ = Nothing
    mapResult f (Done a) = Done $ f a
    mapResult f (Partial a) = Partial $ f a
    mapResult _ Fail = Fail
    mapPartial f (Partial a) = Partial $ f a
    mapPartial _ a = a
