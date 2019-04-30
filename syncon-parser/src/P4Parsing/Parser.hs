{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module P4Parsing.Parser
( Error(..)
, SingleLanguage(..)
, parseSingleLanguage
) where

import Pre
import Result (Result(..))

import Data.Data (Data)
import Control.Monad.Fix (mfix)
import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S

import Text.Earley (Grammar, (<?>), satisfy, rule, Report(..))
import qualified Text.Earley as Earley
import Data.Generics.Uniplate.Data (universe)
import Data.Functor.Foldable (para)

import ErrorMessage (FormatError(..), simpleErrorMessage)

import P1Lexing.Types (Token(..), textualToken, Range, range)
import P1Lexing.Lexer (LanguageTokens(..))
import qualified P1Lexing.Lexer as Lexer
import P2LanguageDefinition.Types (DefinitionFile(..), TypeName(..), Name(..), Syncon(..), Comment(..), Elaboration, SyntaxDescription(..), SyntaxDescriptionF(..), TokenType(..), SDName, SyntaxType, Repetition(..), Rec(..))
import P2LanguageDefinition.Elaborator (elaborate)
import P4Parsing.Types

data Error l
  = LexingError (Lexer.Error l TypeName)
  | UnexpectedToken (HashSet Text) (Tok l)
  | UnexpectedEOF (HashSet Text)
  | MissingTop
  deriving (Show)

instance Show l => FormatError (Error l) where
  formatError (LexingError e) = formatError e
  formatError (UnexpectedToken expected tok) = simpleErrorMessage (range tok) $
    "Unexpected token " <> textualToken tok <> ", expected one of:\n"
    <> (toList expected & sort & foldMap (<> "\n"))
  formatError (UnexpectedEOF expected) = simpleErrorMessage mempty $
    "Unexpected end of file, expected one of:\n"
    <> (toList expected & sort & foldMap (<> "\n"))
  formatError MissingTop = simpleErrorMessage mempty
    "You must define a syntax type named 'Top'."

type Res l = Result [Error l]

data SingleLanguage = SingleLanguage deriving (Show, Eq, Generic, Data, Typeable)
instance Hashable SingleLanguage
type SL = SingleLanguage

type Prod r l a = Earley.Prod r Text (Tok l) a
type Tok l = Token l TypeName

parseSingleLanguage :: DefinitionFile -> Res SL (FilePath -> IO (Res SL (HashSet [Node SL TypeName])))
parseSingleLanguage df = do
  lexFile <- lexer
  parser <- generateGrammar df
  pure $ \path -> do
    resTokens <- lexFile path & fmap (first $ fmap LexingError)
    return $ resTokens >>= \resTokens' -> case Earley.fullParses (Earley.parser $ unquant parser) resTokens' of
      ([], Report{expected, unconsumed = next : _}) -> Error [UnexpectedToken (S.fromList expected) next]
      ([], Report{expected, unconsumed = []}) -> Error [UnexpectedEOF $ S.fromList expected]
      (res, _) -> return $ S.fromList res
  where
    lexer = Lexer.allOneLanguage SingleLanguage (dfToLanguageTokens df) & first (fmap LexingError)

dfToLanguageTokens :: DefinitionFile -> LanguageTokens TypeName
dfToLanguageTokens DefinitionFile{syncons, syntaxTypes, comments} =
  LanguageTokens (S.fromList literals & S.toList) tokens $ c_regex <$> toList comments
  where
    literals = "(" : ")" : do
      Syncon{s_syntaxDescription = descr} <- toList syncons
      SDToken _ t <- universe descr
      pure t
    tokens = do
      Right TokenType{t_name, t_regex} <- toList syntaxTypes
      pure $ (t_name, t_regex)

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

newtype GrammarQuant l = GrammarQuant {unquant :: forall r. Grammar r (Prod r l [Node l TypeName])}

generateGrammar :: DefinitionFile -> Res l (GrammarQuant l)
generateGrammar DefinitionFile{..}
  | not $ (TypeName "Top", S.empty) `S.member` nts = Error [MissingTop]
  | otherwise = Data $ GrammarQuant $ do
      nts' <- mfix $ \nts' -> do
        parens <- M.traverseWithKey (mkParenRule nts') syTyToSyncon
        syncons' <- mapM (generateSyncon markings isSyTy nts' >>> rule) syncons
        return $ forWithKey (S.toMap nts) $ \(tyn, excludes) _ ->
          syncons'
          & (`M.intersection` (S.toMap $ M.lookupDefault S.empty tyn syTyToSyncon))
          & (`M.difference` S.toMap excludes)
          & toList & (lookupEmpty tyn parens : ) & asum & (<?> coerce tyn)
      M.lookup (TypeName "Top", S.empty) nts'
        & compFromJust "P4Parsing.Parser.generateGrammar" "Top somehow vanished during generation"
        & fmap fst
        & many
        & return
  where
    elaboration = elaborate syncons forbids precedences
    markings = mkMarkings syncons isSyTy elaboration
    nts = computeNonTerminals syntaxTypes markings
    syTyToSyncon = computeSynconsBySyntaxType syncons
    mkParenRule nts' tyn _ = rule $ do
      start <- lit "("
      ~(val, _) <- lookupEmpty (tyn, S.empty) nts'
      end <- lit ")"
      pure $ (val, range start <> range end)
    forWithKey = flip M.mapWithKey
    isSyTy tyn = M.lookup tyn syntaxTypes <&> isLeft & fromMaybe False

-- TODO: pass in a value representing the language (for correct token recognizing)
generateSyncon :: HashMap (Name, Either Rec SDName) (TypeName, HashSet Name)
               -> (TypeName -> Bool)
               -> HashMap (TypeName, HashSet Name) (Prod r l (Node l TypeName, Range))
               -> Syncon -> Prod r l (Node l TypeName, Range)
generateSyncon markings isSyTy nts Syncon{s_name = n, s_syntaxDescription, s_syntaxType = (_, syty)} =
  para alg s_syntaxDescription
  & fmap (\(internals, r) -> (Node n (asStruct internals) r, r))
  & (<?> ("syncon " <> coerce n))
  where
    alg (SDNamedF _ sdname (SDSyTy _ tyn, _))
      | isSyTy tyn = M.lookupDefault (tyn, S.empty) (n, Right sdname) markings
        & flip lookupEmpty nts
        & fmap (first $ NodeLeaf >>> Seq.singleton >>> M.singleton sdname >>> Struct)
    alg (SDNamedF _ sdname (SDRec{}, _)) = M.lookupDefault (syty, S.empty) (n, Right sdname) markings
      & flip lookupEmpty nts
      & fmap (first $ NodeLeaf >>> Seq.singleton >>> M.singleton sdname >>> Struct)
    alg sdf = snd <$> sdf & \case
      SDTokenF _ t -> lit t <&> (TokenLeaf &&& range)
      SDSyTyF _ tyn -> if isSyTy tyn
        then lookupEmpty (tyn, S.empty) nts <&> first NodeLeaf
        else othertok tyn <&> (TokenLeaf &&& range)
      SDRecF _ _ -> lookupEmpty (syty, S.empty) nts <&> first NodeLeaf
      SDNamedF _ sdname sd -> sd <&> first (Seq.singleton >>> M.singleton sdname >>> Struct)
      SDRepF _ rep sd -> repF rep sd <&> (unzip >>> (combineMany *** mconcat))
      SDSeqF _ sds -> toList sds & sequenceA & fmap (unzip >>> (combineMany *** mconcat))
      SDAltF _ sds -> toList sds & asum
    repF RepStar = many
    repF RepPlus = some
    repF RepQuestion = optional >>> fmap toList
    combineMany = fmap asStruct >>> foldl' (M.unionWith (<>)) M.empty >>> Struct
    asStruct (Struct s) = s
    asStruct _ = M.empty

-- | Parse a literal
lit :: Text -> Prod r l (Tok l)
lit t = (<?> ("literal " <> t)) . satisfy $ \case
  LitTok _ _ t' | t == t' -> True
  _ -> False

othertok :: TypeName -> Prod r l (Tok l)
othertok tyn = (<?> ("othertok " <> coerce tyn)) . satisfy $ \case
  OtherTok _ _ tyn' _ | tyn == tyn' -> True
  _ -> False

lookupEmpty :: (Eq k, Hashable k, Alternative v) => k -> HashMap k (v a) -> (v a)
lookupEmpty = M.lookupDefault empty

-- TODO: (much later) ensure that the SyntaxDescriptions are formulated in an unambiguous way, after an internal ambiguity check
