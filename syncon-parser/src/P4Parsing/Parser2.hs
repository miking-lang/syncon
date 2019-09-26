{-# LANGUAGE RecordWildCards, ViewPatterns, UndecidableInstances #-}

module P4Parsing.Parser2 (Error(..), precomputeSingleLanguage, dfToLanguageTokens, parseTokens, parseFile, forestToDot) where

import Pre hiding (from, some, many, optional)
import Result (Result(..))

import Control.Monad.Fix (mfix)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Data.Text as Text

import Data.Generics.Uniplate.Data (universe)
import Data.Functor.Foldable (para)

import Text.Earley.Forest.Grammar (terminal, ambig, node, NodeKind(..), Parseable(..), (<--), (-->), noparse, nodeLeaf, label, alts, many, some, optional)
import Text.Earley.Forest.Transformations (mkGrammar, mkNNFGrammar)
import Text.Earley.Forest.Parser (precompute, recognize, mkDerivation, Node)
import qualified Text.Earley.Forest.Grammar as Forest
import qualified Text.Earley.Forest.Parser as Forest

import ErrorMessage (FormatError(..), simpleErrorMessage)

import P1Lexing.Types (Token(..), TokenKind(..), Ranged(..))
import P1Lexing.Lexer (LanguageTokens(..))
import qualified P1Lexing.Lexer as Lexer
import P2LanguageDefinition.Types (DefinitionFile(..), TypeName(..), Name(..), Syncon(..), Comment(..), Elaboration, SyntaxDescription(..), SyntaxDescriptionF(..), TokenType(..), SDName, SyntaxType, Repetition(..), Rec(..))
import P2LanguageDefinition.Elaborator (elaborate)
import P4Parsing.Types (NodeF, SL, SingleLanguage(..))

data Error l tok
  = LexingError (Lexer.Error l TypeName)
  | ParseError (HashSet (Maybe (TokKind tok))) (Maybe tok)
  | MissingTop

instance (Show l, Ranged tok, Parseable tok) => FormatError (Error l tok) where
  formatError _ (LexingError e) = formatError () e
  formatError _ (ParseError expected found) = simpleErrorMessage (foldMap range found) $
    "Parse error, found " <> maybe "<end-of-file>" unlex found <> " expected one of:\n"
    <> Text.unlines (expected & toList <&> \tokKind -> "  " <> maybe "<end-of-file>" (kindLabel @tok) tokKind)
  formatError _ MissingTop = simpleErrorMessage mempty
    "You must define a syntax type named 'Top'."

type Res l tok = Result [Error l tok]

type Prod r l = Forest.Prod r SDName TK
type Grammar r l = Forest.Grammar r Name SDName TK

type TK = TokenKind TypeName

data Precomputed l = Precomputed
  { forest :: !(Forest.Precomputed Name SDName TK)
  , tokens :: !(LanguageTokens TypeName)
  , lexFile :: !(FilePath -> IO (Res l (Token l TypeName) [Token l TypeName]))
  } deriving (Generic)
instance NFData l => NFData (Precomputed l)

precomputeSingleLanguage :: DefinitionFile -> Res SL tok (Precomputed SL)
precomputeSingleLanguage df = do
  let tokens = dfToLanguageTokens df
  lexFileLexErr <- Lexer.allOneLanguage SingleLanguage tokens & first (fmap LexingError)
  let lexFile path = lexFileLexErr path <&> first (fmap LexingError)
  grammar <- generateGrammar df
  let forest = mkGrammar (unquant grammar) & mkNNFGrammar & precompute
  return Precomputed{..}

parseFile :: Precomputed l
          -> FilePath
          -> IO (Res l (Token l TypeName) (HashMap Node (NodeF (Token l TypeName) (HashSet Node)), HashSet Node))
parseFile pc@Precomputed{lexFile} path = do
  resTokens <- lexFile path
  return $ resTokens >>= parseTokens pc

parseTokens :: (Foldable f, Ranged t, Parseable t, TokKind t ~ TK)
            => Precomputed l
            -> f t -> Res l t (HashMap Node (NodeF t (HashSet Node)), HashSet Node)
parseTokens Precomputed{forest} = toList >>> recognize forest >>> mkDerivation forest >>> \case
  Left (Forest.Error expected found) -> Error [ParseError expected found]
  Right res -> Data res

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
newtype GrammarQuant l = GrammarQuant {unquant :: forall r. Grammar r l (Prod r l 'Node)}

-- | The top level generation function.
generateGrammar :: forall l tok. DefinitionFile -> Res l tok (GrammarQuant l)
generateGrammar DefinitionFile{..}
  | not $ (TypeName "Top", S.empty) `S.member` nts = Error [MissingTop]
  | otherwise = Data $ GrammarQuant $ do
      -- The 'mfix' section takes a mapping from high-level non-terminals to their
      -- productions, and then produces that map.
      nts' <- mfix $ \(nts' :: HashMap (TypeName, HashSet Name) (Prod r l 'Node)) -> do
        let parens = M.mapWithKey (mkGrouping nts') syTyToSyncon
        syncons' <- mapM (generateSyncon markings isSyTy nts') syncons
        sequence $ forWithKey (S.toMap nts) $ \(tyn, excludes) _ ->
          syncons'
          & (`M.intersection` (S.toMap $ M.lookupDefault S.empty tyn syTyToSyncon))
          & (`M.difference` S.toMap excludes)
          & toList & (M.lookupDefault [] tyn parens <>) & ambig
      M.lookup (TypeName "Top", S.empty) nts'
        & compFromJust "P4Parsing.Parser.generateGrammar" "Top somehow vanished during generation"
        & return
  where
    elaboration = elaborate syncons forbids precedences
    markings = mkMarkings syncons isSyTy elaboration
    nts = computeNonTerminals syntaxTypes markings
    syTyToSyncon = computeSynconsBySyntaxType syncons
    mkGrouping :: HashMap (TypeName, HashSet Name) (Prod r l 'Node) -> TypeName -> b -> [Prod r l 'Node]
    mkGrouping nts' tyn _ = catMaybes $ toList $ foreach (M.lookup tyn groupings & fold) $ \(open, close) ->
      M.lookup (tyn, S.empty) nts' <&> \prod ->
        either LitKind TypeKind open --> prod <-- either LitKind TypeKind close
    isSyTy :: TypeName -> Bool
    isSyTy tyn = M.lookup tyn syntaxTypes <&> isLeft & fromMaybe False
    forWithKey = flip M.mapWithKey

-- TODO: pass in a value representing the language (for correct token recognizing)
-- | Generate a production for a single 'Syncon'
generateSyncon :: HashMap (Name, Either Rec SDName) (TypeName, HashSet Name)
               -> (TypeName -> Bool)
               -> HashMap (TypeName, HashSet Name) (Prod r l 'Node)
               -> Syncon -> Grammar r l (Prod r l 'Node)
generateSyncon markings isSyTy nts Syncon{s_name = n, s_syntaxDescription, s_syntaxType = (_, syty)} =
  para alg s_syntaxDescription & node n
  where
    alg (SDNamedF _ sdname (SDSyTy _ tyn, _))
      | isSyTy tyn = M.lookupDefault (tyn, S.empty) (n, Right sdname) markings
        & (`M.lookup` nts)
        & maybe noparse (nodeLeaf sdname)
      | otherwise = othertok sdname tyn
    alg (SDNamedF _ sdname (SDRec{}, _)) = M.lookupDefault (syty, S.empty) (n, Right sdname) markings
        & (`M.lookup` nts)
        & maybe noparse (nodeLeaf sdname)
    alg (SDNamedF _ sdname (SDToken _ t, _)) = lit sdname t
    alg sdf = snd <$> sdf & \case
      SDTokenF _ t -> lit_ t
      SDSyTyF _ tyn -> if isSyTy tyn
        then compErr "P4Parsing.Parser2.generateSyncon.alg" "Got an unnamed syty"
        else othertok_ tyn
      SDRecF _ _ -> compErr "P4Parsing.Parser2.generateSyncon.alg" "Got an unnamed rec"
      SDNamedF _ sdname sd -> label sdname sd
      SDRepF _ RepStar sd -> many sd
      SDRepF _ RepPlus sd -> some sd
      SDRepF _ RepQuestion sd -> optional sd
      SDSeqF _ sds -> fold sds
      SDAltF _ sds -> alts $ toList sds

-- | Parse a literal.
lit :: SDName -> Text -> Prod r TK 'Interior
lit sdname = LitKind >>> terminal (Just sdname)

lit_ :: Text -> Prod r TK 'Interior
lit_ = LitKind >>> terminal Nothing

-- | Parse a specific kind of token.
othertok :: SDName -> TypeName -> Prod r TK 'Interior
othertok sdname = TypeKind >>> terminal (Just sdname)

othertok_ :: TypeName -> Prod r TK 'Interior
othertok_ = TypeKind >>> terminal Nothing

-- TODO: (much later) ensure that the SyntaxDescriptions are formulated in an unambiguous way, after an internal ambiguity check

forestToDot :: forall nodeF n. (Foldable nodeF, Functor nodeF, Eq n, Hashable n)
            => (nodeF () -> Text) -> (HashMap n (nodeF (HashSet n)), HashSet n) -> Text
forestToDot showNode (nodeMap, roots) = "digraph {\n"
  <> "  startState [shape=point, label=\"\"];\n"
  <> foldMap (\n -> "  startState -> " <> show (keyToI n) <> ";\n") roots
  <> foldMap nodeDesc (M.toList nodeMap)
  <> nodeEdgeDescs
  <> "}\n"
  where
    keyToIMap = M.keys nodeMap `zip` [(0::Int)..] & M.fromList
    keyToI key = M.lookup key keyToIMap & compFromJust "P4Parsing.ForestParser.forestToDot" "Missing key in keyToIMap"
    nodeDesc (key, realNode) = "  " <> show (keyToI key) <> " [label = \"" <> escape (showNode $ void realNode) <> "\"];\n"
    firstAmbigNode = maximum keyToIMap + 1

    genAlt :: HashSet n -> State (HashMap (HashSet n) Int, Int) (Int, Text)
    genAlt alt = do
      (prevs, id) <- get
      case M.lookup alt prevs of
        Just prevId -> return (prevId, "")
        Nothing -> do
          let altDesc = "  " <> show id <> " [shape=point, label=\"\"];\n"
              edges = "  " <> show id <> " -> {" <> (toList alt <&> keyToI <&> show & intersperse ", " & Text.concat) <> "};\n"
          modify $ M.insert alt id *** (+1)
          return (id, altDesc <> edges)

    nodeEdgeDescs = evalState (foldMapM nodeEdges $ M.toList nodeMap) (M.empty, firstAmbigNode)

    nodeEdges :: (n, nodeF (HashSet n)) -> State (HashMap (HashSet n) Int, Int) Text
    nodeEdges (keyToI -> from, realNode) = flip foldMapM realNode $ \alternatives -> do
      (altId, altDesc) <- genAlt alternatives
      return $ altDesc
        <> "  " <> show from <> " -> " <> show altId <> "[arrowhead=none];\n"

escape :: Text -> Text
escape = Text.concatMap f
  where
    f '\\' = "\\\\"
    f '"' = "\\\""
    f c = Text.singleton c
