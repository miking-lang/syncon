{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module P4Parsing.Generator (programGenerator, PruneShrinkTree(..)) where

import Pre
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import qualified Data.Sequence as Seq
import Data.Bitraversable (bitraverse)

import Data.Functor.Foldable (cata)
import Data.Functor.Foldable.TH (makeBaseFunctor)

import P1Lexing.Types (Token(..), Range(..))
import P2LanguageDefinition.Types (DefinitionFile(..), TypeName(..), Name(..), SDName(..), Syncon(..), SyntaxDescription(..), Rec(..), Repetition(..), SyntaxDescriptionF(..))
import P4Parsing.Types (SingleLanguage(..))
import P2LanguageDefinition.Elaborator (elaborate)

import Hedgehog (Gen)
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

data CSTNode = CSTNode
  { cst_name :: !(Maybe Name)  -- Nothing means grouping parens
  , cst_type :: !TypeName
  , cst_disallowedHere :: !(HashSet Name)
  , cst_children :: !(Seq (Either Tok CSTNode))
  }
type Tok = Token SingleLanguage TypeName
makeBaseFunctor ''CSTNode

data PruneShrinkTree = PruneShrinkTree | DoNotPruneShrinkTree
data AllowRecursion = AllowRecursion | DisallowRecursion

findNonRecSyncons :: (TypeName -> Bool) -> (TypeName -> Seq (Maybe Name, SyntaxDescription)) -> HashSet TypeName -> HashMap TypeName (Seq (Maybe Name, SyntaxDescription))
findNonRecSyncons isToken getSyns types = recur mempty
  where
    getTargetTypesAlg :: TypeName -> SyntaxDescriptionF (HashSet TypeName) -> HashSet TypeName
    getTargetTypesAlg _ (SDSyTyF _ tyn) = S.singleton tyn
    getTargetTypesAlg tyn SDRecF{} = S.singleton tyn
    getTargetTypesAlg _ sd = fold sd

    getTargets :: TypeName -> SyntaxDescription -> HashSet TypeName
    getTargets tyn synDesc =
      cata (getTargetTypesAlg tyn) synDesc
      & S.filter (not . isToken)

    recur :: HashMap TypeName (Seq (Maybe Name, SyntaxDescription)) -> HashMap TypeName (Seq (Maybe Name, SyntaxDescription))
    recur prev
      | all (`M.member` prev) types = prev
      | prev == next = prev
      | otherwise = recur next
      where
        next = foldl' (\m tyn -> M.alter (updateTyn tyn) tyn m) prev types
        updateTyn :: TypeName -> Maybe (Seq (Maybe Name, SyntaxDescription)) -> Maybe (Seq (Maybe Name, SyntaxDescription))
        updateTyn _ x@Just{} = x
        updateTyn tyn Nothing = getSyns tyn
          & Seq.filter (snd >>> getTargets tyn >>> all (`M.member` prev))
          & \case
          Seq.Empty -> Nothing
          x -> Just x

programGenerator :: PruneShrinkTree -> DefinitionFile -> Gen (Int, HashSet Name, HashSet TypeName, Seq Tok)
programGenerator prune DefinitionFile{syncons, forbids, precedences, groupings, syntaxTypes, precedenceKind} =
  genInSyTy prune getDisallowed getSyns' isToken (TypeName "Top") S.empty
  <&> (cata combinedAlg >>> coerce)
  where
    elaboration = elaborate syncons forbids precedences precedenceKind
    getDisallowed :: (Name, Either Rec SDName) -> HashSet Name
    getDisallowed loc = M.lookupDefault S.empty loc elaboration

    isToken :: TypeName -> Bool
    isToken tyn = M.lookup tyn syntaxTypes <&> isRight & fromMaybe False

    getSyns' :: AllowRecursion -> TypeName -> Seq (Maybe Name, SyntaxDescription)
    getSyns' DisallowRecursion = \tyn -> M.lookup tyn nonRecBySyTy & compFromJust "Generator.programGenerator.getSyns'" ("Couldn't find a list for " <> show tyn)
    getSyns' AllowRecursion = getSyns

    nonRecBySyTy :: HashMap TypeName (Seq (Maybe Name, SyntaxDescription))
    nonRecBySyTy = findNonRecSyncons isToken getSyns $ S.fromMap $ void $ M.mapMaybe leftToMaybe syntaxTypes

    getSyns :: TypeName -> Seq (Maybe Name, SyntaxDescription)
    getSyns tyn = M.lookupDefault mempty tyn allBySyTy
    allBySyTy = M.unionWith (<>) synBySyTy groupSynBySyTy
    synBySyTy = toList syncons
      <&> (\Syncon{s_name, s_syntaxType, s_syntaxDescription} -> (snd s_syntaxType, Seq.singleton (Just s_name, s_syntaxDescription)))
      & M.fromListWith (<>)
    groupSynBySyTy = groupings
      <&> fmap (groupingToSyn >>> (Nothing,))
    groupingToSyn (open, close) = SDSeq Nowhere $ Seq.fromList
      [ either (SDToken Nowhere) (SDSyTy Nowhere) open
      , SDRec Nowhere Rec
      , either (SDToken Nowhere) (SDSyTy Nowhere) close ]

genInSyTy :: PruneShrinkTree
          -> ((Name, Either Rec SDName) -> HashSet Name)
          -> (AllowRecursion -> TypeName -> Seq (Maybe Name, SyntaxDescription))
          -> (TypeName -> Bool)
          -> TypeName
          -> HashSet Name
          -> Gen CSTNode
genInSyTy prune getDisallowed getSyns isToken tyn disallowed = G.sized $ \n ->
  let filtered = getSyns (if n > 1 then AllowRecursion else DisallowRecursion) tyn
        & toList
        & filter (fst >>> maybe False (not . (`S.member` disallowed)))
      gens = catMaybes $ mkGen <$> filtered
  in G.choice gens
  where
    mkGen :: (Maybe Name, SyntaxDescription) -> Maybe (Gen CSTNode)
    mkGen (mName, synconSd) = recur synconSd
      <&> fmap (CSTNode mName tyn disallowed)
      <&> doPrune
      <&> G.shrink (shrinkCST >>> drop 1)
      where
        getDis :: Either Rec SDName -> HashSet Name
        getDis = case mName of
          Just name -> (name,) >>> getDisallowed
          Nothing -> const S.empty

        doPrune = case prune of
          PruneShrinkTree -> G.prune
          DoNotPruneShrinkTree -> identity

        recGen :: Maybe SDName -> Maybe Rec -> TypeName -> Maybe (Gen (Seq (Either Tok CSTNode)))
        recGen msd mrec ctyn@(TypeName ctyn')
          | isToken ctyn = Just $ pure $ Seq.singleton $ Left $
            OtherTok Nowhere SingleLanguage ctyn ("<" <> ctyn' <> ">")
          | otherwise =
            foldMap (Right >>> getDis) msd
            <> foldMap (Left >>> getDis) mrec
            & genInSyTy prune getDisallowed getSyns isToken ctyn
            & G.small
            <&> Right
            <&> Seq.singleton
            & Just

        recur :: SyntaxDescription -> Maybe (Gen (Seq (Either Tok CSTNode)))
        recur (SDSeq _ sds) = mapM recur sds <&> fold
        recur (SDAlt _ sds) = toList sds & mapM recur <&> G.choice
        recur (SDRep _ RepStar sd) = recur sd <&> G.seq (R.linear 0 5) <&> fmap fold
        recur (SDRep _ RepPlus sd) = recur sd <&> G.seq (R.linear 1 5) <&> fmap fold
        recur (SDRep _ RepQuestion sd) = recur sd <&> G.maybe <&> fmap fold
        recur (SDNamed _ sdname (SDSyTy _ ctyn)) = recGen (Just sdname) Nothing ctyn
        recur (SDNamed _ sdname (SDRec _ r)) = recGen (Just sdname) (Just r) tyn
        recur (SDNamed _ _ sd) = recur sd
        recur (SDSyTy _ ctyn) = recGen Nothing Nothing ctyn
        recur (SDRec _ r) = recGen Nothing (Just r) tyn
        recur (SDToken _ t) = Just $ pure $ Seq.singleton $ Left $ LitTok Nowhere SingleLanguage t

-- Invariant: the first element in the list is the unchanged CST, all others are shrunk
shrinkCST :: CSTNode -> [CSTNode]
shrinkCST n@CSTNode{cst_children} =
  let childShrunk = [n {cst_children = children} | children <- mapM (bitraverse pure shrinkCST) cst_children]
  in childShrunk <> (childShrunk >>= shallowShrinkCST)

shallowShrinkCST :: CSTNode -> [CSTNode]
shallowShrinkCST CSTNode{cst_type, cst_disallowedHere, cst_children} =
  cst_children
  & toList
  & mapMaybe rightToMaybe
  & filter (\CSTNode{cst_type = ty, cst_name} -> cst_type == ty
             && maybe True (\n -> not $ n `S.member` cst_disallowedHere) cst_name)
  <&> (\c -> c {cst_disallowedHere = cst_disallowedHere})

combinedAlg :: CSTNodeF (Sum Int, HashSet Name, HashSet TypeName, Seq Tok) -> (Sum Int, HashSet Name, HashSet TypeName, Seq Tok)
combinedAlg cst = (sizeAlg $ p1 <$> cst, nameAlg $ p2 <$> cst, typeAlg $ p3 <$> cst, flattenAlg $ p4 <$> cst)
  where
    p1 (a, _, _, _) = a
    p2 (_, a, _, _) = a
    p3 (_, _, a, _) = a
    p4 (_, _, _, a) = a

flattenAlg :: CSTNodeF (Seq Tok) -> Seq Tok
flattenAlg = cst_childrenF >=> either Seq.singleton identity

sizeAlg :: CSTNodeF (Sum Int) -> Sum Int
sizeAlg CSTNodeF{cst_childrenF} = 1 + foldMap fold cst_childrenF

nameAlg :: CSTNodeF (HashSet Name) -> HashSet Name
nameAlg CSTNodeF{cst_childrenF, cst_nameF} =
  foldMap fold cst_childrenF & maybe identity S.insert cst_nameF

typeAlg :: CSTNodeF (HashSet TypeName) -> HashSet TypeName
typeAlg CSTNodeF{cst_childrenF, cst_typeF, cst_disallowedHereF = _} =
  foldMap (either getTokTy identity) cst_childrenF & S.insert cst_typeF
  where
    getTokTy LitTok{} = S.empty
    getTokTy (OtherTok _ _ tyn _) = S.singleton tyn
