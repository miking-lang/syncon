{-# LANGUAGE RecordWildCards #-}

module P4Parsing.Parser2 where

import Pre
import Result (Result(..))

import Control.Monad.Fix (mfix)
import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S

import Data.Generics.Uniplate.Data (universe)
import Data.Functor.Foldable (para)

import ErrorMessage (FormatError(..), simpleErrorMessage)
import qualified P4Parsing.ForestParser as Forest
import P4Parsing.ForestParser (Node, terminal, ambig, rule, parse)

import P1Lexing.Types (Token(..), Range(Nowhere), range)
import P1Lexing.Lexer (LanguageTokens(..))
import qualified P1Lexing.Lexer as Lexer
import P2LanguageDefinition.Types (DefinitionFile(..), TypeName(..), Name(..), Syncon(..), Comment(..), Elaboration, SyntaxDescription(..), SyntaxDescriptionF(..), TokenType(..), SDName, SyntaxType, Repetition(..), Rec(..))
import P2LanguageDefinition.Elaborator (elaborate)
import P4Parsing.Types (pattern NodeF, SL, NodeInternals(..), SingleLanguage(..))
import qualified P4Parsing.Types as P4

data Error l
  = LexingError (Lexer.Error l TypeName)
  | ParseError Text
  | MissingTop
  deriving (Show)

instance Show l => FormatError (Error l) where
  formatError _ (LexingError e) = formatError () e
  formatError _ (ParseError t) = simpleErrorMessage mempty $
    "Parse error, and the current parser gives the error as a string, thus the\n"
    <> "below error is taken verbatim, and is likely hard to understand, for now. :(\n\n"
    <> t
  formatError _ MissingTop = simpleErrorMessage mempty
    "You must define a syntax type named 'Top'."

type Res l = Result [Error l]

type NodeF l = P4.NodeF l TypeName

type Prod r l a = Forest.Prod r (Tok l) (NodeF l) a
type Grammar r l a = Forest.Grammar r (Tok l) (NodeF l) a
type Tok l = Token l TypeName

parseSingleLanguage :: DefinitionFile -> Res SL (FilePath -> IO (Res SL (HashMap Node (NodeF SL (HashSet Node)), HashSet Node)))
parseSingleLanguage df = do
  lexFile <- lexer
  grammar <- generateGrammar df
  pure $ \path -> do
    resTokens <- lexFile path <&> first (fmap LexingError)
    return $ resTokens >>= \resTokens' -> case parse (unquant grammar) resTokens' of
      Left err -> Error [ParseError err]
      Right forest -> return forest
  where
    lexer = Lexer.allOneLanguage SingleLanguage (dfToLanguageTokens df) & first (fmap LexingError)

-- | Look up keywords, comment syntax, etc., to produce the parameters for the lexer.
dfToLanguageTokens :: DefinitionFile -> LanguageTokens TypeName
dfToLanguageTokens DefinitionFile{syncons, syntaxTypes, comments, groupings} =
  LanguageTokens (literals <> groupLits & S.fromList & S.toList) tokens $ (c_beginRegex &&& c_endRegex) <$> toList comments
  where
    literals = do
      Syncon{s_syntaxDescription = descr} <- toList syncons
      SDToken _ t <- universe descr
      pure t
    groupLits = do
      (open, close) <- toList groupings >>= toList
      either pure (const []) open <> either pure (const []) close
    tokens = do
      Right TokenType{t_name, t_regex} <- toList syntaxTypes
      pure $ (t_name, t_regex)

-- | Compute the markings for each occurrence of a syntax type in a syntax description.
-- The resulting 'HashMap' may not be total, and should thus be used through M.lookupDefault S.empty.
-- When a syntax description contains "name:rec", the entry for "name" may contain a superset of
-- the set for "rec", so use the former.
mkMarkings :: HashMap Name Syncon -> (TypeName -> Bool) -> Elaboration -> HashMap (Name, Either Rec SDName) (TypeName, HashSet Name)
mkMarkings syncons isSyTy elaboration = M.filter (fst >>> isSyTy) tynames
  where
    tynames = M.fromList $ do
      (n, syncon) <- M.toList syncons
      universe (s_syntaxDescription syncon) >>= \case
        SDNamed _ sdname (SDSyTy _ tyn) ->
          pure ( (n, Right sdname)
               , (tyn, getMarkings (n, Right sdname)) )
        SDNamed _ sdname (SDRec _ r) ->
          pure ( (n, Right sdname)
               , ( s_syntaxType syncon & snd
                 , getMarkings (n, Right sdname) <> getMarkings (n, Left r) ) )
        SDRec _ r ->
          pure ( (n, Left r)
               , (s_syntaxType syncon & snd
                 , getMarkings (n, Left r) ) )
        _ -> []
    getMarkings :: (Name, Either Rec SDName) -> HashSet Name
    getMarkings k = M.lookupDefault S.empty k elaboration

-- | Compute all the high-level non-terminals in the resulting grammar, i.e., a set of
-- marked syntax types.
computeNonTerminals :: HashMap TypeName (Either SyntaxType TokenType)
                    -> HashMap (Name, Either Rec SDName) (TypeName, HashSet Name)
                    -> HashSet (TypeName, HashSet Name)
computeNonTerminals types markings = unmarked <> toList markings & S.fromList
  where
    unmarked = do
      (tyn, Left _) <- M.toList types
      pure (tyn, S.empty)

computeSynconsBySyntaxType :: Foldable f => f Syncon -> HashMap TypeName (HashSet Name)
computeSynconsBySyntaxType syncons = M.fromListWith S.union $ do
  Syncon{s_name, s_syntaxType} <- toList syncons
  pure (snd s_syntaxType, S.singleton s_name)

-- | Dummy wrapper to allow a forall quantified type inside 'Result'.
newtype GrammarQuant l = GrammarQuant {unquant :: forall r. Grammar r l (Prod r l r)}

-- | The top level generation function.
generateGrammar :: forall l. DefinitionFile -> Res l (GrammarQuant l)
generateGrammar DefinitionFile{..}
  | not $ (TypeName "Top", S.empty) `S.member` nts = Error [MissingTop]
  | otherwise = Data $ GrammarQuant $ do
      -- The 'mfix' section takes a mapping from high-level non-terminals to their
      -- productions, and then produces that map.
      nts' <- mfix $ \(nts' :: HashMap (TypeName, HashSet Name) (Prod r l r)) -> do
        let parens = M.mapWithKey (mkGrouping nts') syTyToSyncon
        syncons' <- mapM (generateSyncon markings isSyTy nts' >>> rule) syncons
        sequence $ forWithKey (S.toMap nts) $ \(tyn, excludes) _ ->
          syncons'
          & (`M.intersection` (S.toMap $ M.lookupDefault S.empty tyn syTyToSyncon))
          & (`M.difference` S.toMap excludes)
          & toList & (lookupEmpty tyn parens : ) & ambig
      M.lookup (TypeName "Top", S.empty) nts'
        & compFromJust "P4Parsing.Parser.generateGrammar" "Top somehow vanished during generation"
        & return
  where
    elaboration = elaborate syncons forbids precedences
    markings = mkMarkings syncons isSyTy elaboration
    nts = computeNonTerminals syntaxTypes markings
    syTyToSyncon = computeSynconsBySyntaxType syncons
    mkGrouping :: HashMap (TypeName, HashSet Name) (Prod r l r) -> TypeName -> b -> Prod r l r
    mkGrouping nts' tyn _ = asum $ foreach (M.lookup tyn groupings & fold) $ \(open, close) ->
      either lit othertok open *> lookupEmpty (tyn, S.empty) nts' <* either lit othertok close
    isSyTy :: TypeName -> Bool
    isSyTy tyn = M.lookup tyn syntaxTypes <&> isLeft & fromMaybe False
    forWithKey = flip M.mapWithKey

-- TODO: pass in a value representing the language (for correct token recognizing)
-- | Generate a production for a single 'Syncon'
generateSyncon :: HashMap (Name, Either Rec SDName) (TypeName, HashSet Name)
               -> (TypeName -> Bool)
               -> HashMap (TypeName, HashSet Name) (Prod r l r)
               -> Syncon -> Prod r l (NodeF l r)
generateSyncon markings isSyTy nts Syncon{s_name = n, s_syntaxDescription, s_syntaxType = (_, syty)} =
  para alg s_syntaxDescription
  <&> asStruct
  <&> NodeF n
  & ranged
  where
    alg (SDNamedF _ sdname (SDSyTy _ tyn, _))
      | isSyTy tyn = M.lookupDefault (tyn, S.empty) (n, Right sdname) markings
        & flip lookupEmpty nts
        <&> (NodeLeaf >>> Seq.singleton >>> M.singleton sdname >>> Struct)
    alg (SDNamedF _ sdname (SDRec{}, _)) = M.lookupDefault (syty, S.empty) (n, Right sdname) markings
      & flip lookupEmpty nts
      <&> (NodeLeaf >>> Seq.singleton >>> M.singleton sdname >>> Struct)
    alg sdf = snd <$> sdf & \case
      SDTokenF _ t -> lit t <&> TokenLeaf
      SDSyTyF _ tyn -> if isSyTy tyn
        then lookupEmpty (tyn, S.empty) nts <&> NodeLeaf
        else othertok tyn <&> TokenLeaf
      SDRecF _ _ -> lookupEmpty (syty, S.empty) nts <&> NodeLeaf
      SDNamedF _ sdname sd -> sd <&> (Seq.singleton >>> M.singleton sdname >>> Struct)
      SDRepF _ rep sd -> repF rep sd <&> combineMany
      SDSeqF _ sds -> toList sds & sequenceA <&> combineMany
      SDAltF _ sds -> toList sds & asum
    repF RepStar = many
    repF RepPlus = some
    repF RepQuestion = optional >>> fmap toList
    combineMany = fmap asStruct >>> foldl' (M.unionWith (<>)) M.empty >>> Struct
    asStruct (Struct s) = s
    asStruct _ = M.empty

ranged :: Prod r l (Range -> a) -> Prod r l a
ranged = fmap (mkRange >>>) >>> Forest.ranged
  where
    mkRange Nothing = Nowhere
    mkRange (Just (a, b)) = range a <> range b

-- | Parse a literal.
lit :: Text -> Prod r l (Tok l)
lit t = terminal ("literal " <> t) $ \case
  LitTok _ _ t' | t == t' -> True
  _ -> False

-- | Parse a specific kind of token.
othertok :: TypeName -> Prod r l (Tok l)
othertok tyn = terminal ("token " <> coerce tyn) $ \case
  OtherTok _ _ tyn' _ | tyn == tyn' -> True
  _ -> False

-- | Total lookup in a map, defaulting to 'empty' from 'Alternative', i.e., a parser that fails.
lookupEmpty :: (Eq k, Hashable k, Alternative v) => k -> HashMap k (v a) -> (v a)
lookupEmpty = M.lookupDefault empty

-- TODO: (much later) ensure that the SyntaxDescriptions are formulated in an unambiguous way, after an internal ambiguity check
