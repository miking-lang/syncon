{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module P2LanguageDefinition.BasicChecker
( Error(..)
, mkDefinitionFile
) where

import Pre hiding (check)
import Result (Result(..), errorIfNonEmpty)

import qualified Data.Text as Text
import Data.Bitraversable (bisequence)
import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S

import ErrorMessage (ErrorMessage(..), FormatError(..), simpleErrorMessage)

import Data.Generics.Uniplate.Data (universe)

import P1Lexing.Types (Range, range, Ranged)
import P2LanguageDefinition.Types

type Res = Result [Error]
data Error
  = DuplicateDefinition Text [Range]
  | Undefined Text Range
  | NotAnOperator Name Range
  | InconsistentPrecedence (Name, Name) [(Ordering, HashSet Range)]
  | NonEqSelfPrecedence Name (HashSet Range)
  | WrongSyntaxType Name SDName TypeName Name TypeName Range  -- ^ name.sdname : syntaxtype, but name : typename
  | NotASyntaxTypeOccurrence Name SDName Range
  | NotAllSameSyntaxType (HashMap TypeName (HashSet Name)) Range
  deriving (Show, Eq)

instance FormatError Error where
  formatError (DuplicateDefinition defname ranges) = ErrorMessage
    { e_message = defname <> " is defined multiple times."
    , e_range = fold ranges
    , e_ranges = sort ranges `zip` [1 :: Int ..]
      & fmap (second $ show >>> ("Definition " <>))
    }
  formatError (Undefined defname r) = simpleErrorMessage r $
    defname <> " is undefined"
  formatError (NotAnOperator defname r) = simpleErrorMessage r $
    coerce defname <> " is not an operator (it was defined with 'syncon', not 'infix', 'prefix', or 'suffix')."
  formatError (InconsistentPrecedence (n1, n2) defs) = ErrorMessage
    { e_message = coerce n1 <> " and " <> coerce n2 <> " have inconsistent precedences."
    , e_range = foldMap (snd >>> fold) defs
    , e_ranges = defs >>= \(ordering, rs) -> (, fmt ordering) <$> toList rs
    }
    where
      fmt LT = coerce n1 <> " < " <> coerce n2
      fmt EQ = coerce n1 <> " = " <> coerce n2
      fmt GT = coerce n1 <> " > " <> coerce n2
  formatError (NonEqSelfPrecedence defname rs) = ErrorMessage
    { e_message = coerce defname <> " is declared to have higher precedence than itself."
    , e_range = fold rs
    , e_ranges = toList rs <&> (, "")
    }
  formatError (WrongSyntaxType n1 (SDName sdn) tyn1 n2 tyn2 r) = simpleErrorMessage r $
    coerce n1 <> "." <> sdn <> " has syntax type " <> coerce tyn1 <> ", but "
    <> coerce n2 <> " has syntax type " <> coerce tyn2 <> "."
  formatError (NotASyntaxTypeOccurrence n (SDName sdn) r) = simpleErrorMessage r $
    coerce n <> "." <> sdn <> " is not declared as a syntax type, cannot forbid."
  formatError (NotAllSameSyntaxType typePartitions r) = simpleErrorMessage r $
    "All syncons in a precedence list must share the same syntax type.\n\n" <> formatted
    where
      formatted = typePartitions <&> (toList >>> fmap coerce >>> Text.intercalate ", ")
        & M.toList
        & foldMap (\(tyn, syncons) -> coerce tyn <> ":\n  " <> syncons <> "\n")

addImplicits :: [Top] -> [Top]
addImplicits tops = SyntaxTypeTop (SyntaxType (TypeName "Top") mempty) : tops

-- | This will coalesce all the top-level declarations, make sure that each of them
-- is valid. This validity includes things like checking that regexes are correct,
-- that nothing is defined twice, that no syntax type used is undefined, etc.
-- The check does not check for any kind of ambiguity, neither internal nor unresolvable.
--
-- NOTE: this will not check the validity of regular expressions
mkDefinitionFile :: [Top] -> Res DefinitionFile
mkDefinitionFile (addImplicits >>> Seq.fromList -> tops) = do
  findDuplicates s_name synconTops
  findDuplicates getTypeName typeTops
  traverse_ (checkSyncon typeNames) synconTops
  traverse_ (checkForbid synconAndSDNames) forbids
  precedences <- checkPrecedences synconAndSDNames tops
  pure $ DefinitionFile{..}
  where
    typeTops = toList tops & mapMaybe asTypeDef & Seq.fromList
    synconTops = Seq.fromList [s | SynconTop s <- toList tops]
    forbids = Seq.fromList [f | ForbidTop f <- toList tops]
    comments = Seq.fromList [c | CommentTop c <- toList tops]

    typeNames = void syntaxTypes & S.fromMap
    synconAndSDNames = (\syn -> (snd $ s_syntaxType syn, getSDRecs syn, getSDNames syn)) <$> syncons

    syncons = mkMap s_name synconTops
    syntaxTypes = mkMap getTypeName typeTops

    asTypeDef (SyntaxTypeTop st) = Just $ Left st
    asTypeDef (TokenTypeTop tt) = Just $ Right tt
    asTypeDef _ = Nothing
    getTypeName (Left SyntaxType{st_name}) = st_name
    getTypeName (Right TokenType{t_name}) = t_name

-- | Find all errors local to a single syncon
checkSyncon :: HashSet TypeName -> Syncon -> Res ()
checkSyncon types Syncon{..} = do
  unless (snd s_syntaxType `S.member` types) $
    Error [Undefined (coerce $ snd s_syntaxType) $ fst s_syntaxType]
  for_ [(r, tyn) | SDSyTy r tyn <- universe s_syntaxDescription] $ \(r, tyn) ->
    unless (tyn `S.member` types) $ Error [Undefined (coerce tyn) r]
  findDuplicates fst [(t, r) | SDNamed r (SDName t) _ <- universe s_syntaxDescription]
  pure ()

-- | Check that forbids refer to actually defined things, and that the syntax types agree
checkForbid :: HashMap Name (TypeName, HashSet Rec, HashMap SDName (Maybe TypeName)) -> Forbid -> Res ()
checkForbid names = \case
  Forbid _ n1 (sdr, sdname) n2 -> do
    ~(_, _, sdnames) <- lookupName n1
    ~(n2ty, _, _) <- lookupName n2
    case M.lookup sdname sdnames of
      Nothing -> Error [Undefined (toText sdname) sdr]
      Just Nothing -> Error [NotASyntaxTypeOccurrence (snd n1) sdname $ fst n1 <> sdr]
      Just (Just n1ty) | n2ty == n1ty -> pure ()
      Just (Just n1ty) -> Error [WrongSyntaxType (snd n1) sdname n1ty (snd n2) n2ty $ fst n2]
    pure ()
  ForbidRec _ n1 _ n2 -> do
    ~(n1ty, _, _) <- lookupName n1
    ~(n2ty, _, _) <- lookupName n2
    unless (n1ty == n2ty) $
      Error [WrongSyntaxType (snd n1) (SDName "rec") n1ty (snd n2) n2ty $ fst n2]
    pure ()
  where
    lookupName (nr, n) = case M.lookup n names of
      Just info -> Data info
      Nothing -> Error [Undefined (coerce n) nr]
    toText (SDName t) = t

-- | Check that precedence is consistent, and that a single precedence list only defines
-- precedence for a single syntax type
checkPrecedences :: Foldable t
                 => HashMap Name (TypeName, HashSet Rec, HashMap SDName (Maybe TypeName))
                 -> t Top
                 -> Res PrecedenceMatrix
checkPrecedences names tops = consistentTypes *> matrix
  where
    precedences = Seq.fromList [pl | PrecedenceTop pl <- toList tops]

    consistentTypes = traverse_ operatorTypes precedences
    operatorTypes (PrecedenceList r pList eList) = foldMap toList pList <> foldMap toList eList
      & S.fromList & S.toList
      & traverse (operatorType r)
      & fmap (M.fromListWith S.union)
      & (>>= \types -> case M.toList types of
                 [_] -> pure ()
                 _ -> Error [NotAllSameSyntaxType types r])
    operatorType r n@(Name t) = case M.lookup n names of
      Nothing -> Error [Undefined t r]
      Just (_, recs, _) | S.null recs -> Error [NotAnOperator n r]
      Just (ty, _, _) -> Data (ty, S.singleton n)

    matrix = precedences
      & fmap mat
      & foldl' mergePreMatrices M.empty
      & M.traverseWithKey checkEntry
      & fmap (M.mapMaybe identity >>> PrecedenceMatrix)
    mat (PrecedenceList r pList eList) =
      mergePreMatrices gtPrecedences eqPrecedences `M.difference` exceptions
      where
        gtPrecedences = ((toList <$> pList & orderedPairs)
          >>= uncurry (liftA2 $ mkEntry GT))
          & foldl' mergePreMatrices M.empty
        eqPrecedences = (toList pList >>= orderedPairs)
          & fmap (uncurry $ mkEntry EQ)
          & foldl' mergePreMatrices M.empty
        exceptions = foldMap orderedPairs eList & S.fromList & S.toMap
        mkEntry ordering n1@(Name t1) n2@(Name t2)
          | t1 <= t2 = M.singleton (n1, n2) $ M.singleton ordering $ S.singleton r
          | otherwise = M.singleton (n2, n1) $ M.singleton (reverseOrdering ordering) $ S.singleton r

    checkEntry :: (Name, Name) -> HashMap Ordering (HashSet Range) -> Res (Maybe (Ordering, HashSet Range))
    checkEntry pair@(n1, n2) (M.toList -> entries)
      | n1 /= n2, [entry] <- entries = pure $ Just entry
      | n1 == n2, [(EQ, _)] <- entries = pure Nothing  -- NOTE: an operator always has equal precedence with itself, don't store it
      | n1 == n2, [(_, rs)] <- entries = Error [NonEqSelfPrecedence n1 rs]
      | otherwise = Error [InconsistentPrecedence pair entries]

    reverseOrdering LT = GT
    reverseOrdering EQ = EQ
    reverseOrdering GT = LT

    mergePreMatrices = M.unionWith $ M.unionWith S.union

    -- | Given a sequence of 'a's, produce a list of all pairs (a, a) such that the first component
    -- appears earlier in the sequence than the second.
    orderedPairs :: Foldable t => t a -> [(a, a)]
    orderedPairs = (toList >>> tails)
      >=> (uncons
           >>> fmap (first pure >>> bisequence)
           >>> fold)

-- |
-- = Helpers

-- | Find all duplicate definitions, given a name function and a collection of definitions
findDuplicates :: (Ranged a, Coercible b Text, Eq b, Hashable b, Foldable t) => (a -> b) -> t a -> Res ()
findDuplicates getName tops = (getName &&& (range >>> pure)) <$> toList tops
  & M.fromListWith (<>)
  & M.filter (length >>> (> 1))
  & M.toList
  & fmap (\(t, ranges) -> DuplicateDefinition (coerce t) ranges)
  & errorIfNonEmpty

-- | Extract all 'SDName's defined in the syntax description of a syncon
getSDNames :: Syncon -> HashMap SDName (Maybe TypeName)
getSDNames Syncon{s_syntaxDescription, s_syntaxType = (_, selfTy)} =
  inner s_syntaxDescription & M.fromList
  where
    inner descr = universe descr
      & mapMaybe (\case
                     SDNamed _ sdname (SDSyTy _ ty) -> Just (sdname, Just ty)
                     SDNamed _ sdname SDRec{} -> Just (sdname, Just selfTy)
                     SDNamed _ sdname _ -> Just (sdname, Nothing)
                     _ -> Nothing)

-- | Find all 'Rec's present in the syntax description of a syncon
getSDRecs :: Syncon -> HashSet Rec
getSDRecs = s_syntaxDescription >>> inner >>> S.fromList
  where
    inner descr = [r | SDRec _ r <- universe descr]

-- | Make a 'HashMap' given a function to generate a key and a collection of values
mkMap :: (Eq b, Hashable b, Foldable t) => (a -> b) -> t a -> HashMap b a
mkMap getName = toList >>> fmap (getName &&& identity) >>> M.fromList
