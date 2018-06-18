{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module ImplChecker (check, Error(..)) where

import Debug.Trace

import Control.Monad (zipWithM)
import Control.Arrow ((&&&), (***))

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Function ((&))
import Data.Foldable (foldlM)
import Data.Either (partitionEithers)
import Data.Functor.Identity (runIdentity)
import Data.Semigroup (Semigroup, (<>))

import qualified Binding

import Types.Construction (Construction(..), SyntaxPattern(..), Repeat(..))
import Types.ResolvedConstruction (ResolvedConstruction, expand)
import Types.Ast (MidNodeI(..), NodeI(..), FixNode(..))
import Types.GenSym (GenSym(..))
import Types.Result (ResultT(..))
import Types.Lexer (Range, range)

type Node s = FixNode s GenSym
type MidNode s = MidNodeI (s (Node s)) GenSym

-- TODO: no check is done on correct usage of fold1, do that here or in construction resolution?

-- First String is name of syntax construction
           -- FakeNode can bind names exposed through Dependency before expansion, but not after
data Error = DependencyError String FakeNode Dependency
           -- FakeNode can see names exported from itself after expansion, can give redef errors
           | SelfDependentError String FakeNode
           -- Something was exported (#bind before, or #bind after) before expansion, but isn't after
           | ExportExpectedError String Export
           -- Regular binding errors, possibly by not exposing an Identifier from the syntax description
           | BindingError String Binding.Error
           -- Syntaxtype error, (got, range), (expected, parent range)
           | TypeError String (String, Range) (String, Range)
           deriving (Show, Eq, Ord)

check :: [(String, Maybe String)] -> M.Map String (Construction n) -> M.Map String ResolvedConstruction -> S.Set Error
check tys constrs resolvedConstrs = mconcat . M.elems $
  M.intersectionWithKey
    (doCheck typeMap constrs resolvedConstrs)
    expansionFunctions
    syntaxDescriptions
  where
    syntaxDescriptions = syntax <$> constrs
    expansionFunctions = expand `M.mapMaybe` resolvedConstrs
    typeMap = mkTypeMap tys

data FakeNodeS s = FakeNodeS FakeNode deriving (Show)
data FakeNode = FakeNode FakeKind [Int] deriving (Eq, Ord)
data FakeKind = SyntaxKind String | OtherKind deriving (Eq, Ord)

instance Show FakeNode where
  show (FakeNode kind path) = show kind ++ "#" ++ show (reverse path)
instance Show FakeKind where
  show (SyntaxKind ty) = "syn[" ++ ty ++ "]"
  show OtherKind = "other"

doCheck :: M.Map String String -> M.Map String (Construction n) -> M.Map String (ResolvedConstruction) -> String -> (Node FakeNodeS -> MidNode FakeNodeS) -> [SyntaxPattern] -> S.Set Error
doCheck typeMap constrs resConstrs constrName _expand pats = S.unions $ inner <$> pairs
  where
    inner (start, expansion) =
      let (startErrs, (fmap runIdentity -> startDeps, startExports)) = computeDependencies resConstrs start
          (expansionErrs, (expansionDeps, expansionExports)) = computeDependencies resConstrs expansion
          bindingErrors = S.mapMonotonic (BindingError constrName) $ expansionErrs `S.difference` startErrs
          dependencyErrors = S.unions . M.elems $ depErrIntersect startDeps expansionDeps
          exportErrors = S.mapMonotonic (ExportExpectedError constrName) $ startExports `S.difference` expansionExports
          typeErrors = checkTypes typeMap constrs start expansion
      in S.unions [bindingErrors, dependencyErrors, exportErrors, typeErrors]
    depErrIntersect = M.intersectionWithKey $ \n req (sat :: [S.Set Dependency]) ->
      let unsatisfied = S.mapMonotonic (DependencyError constrName n) . S.unions $ S.difference req <$> sat
          selfDependent = if any (\case {SyntaxDependency n' _ -> n == n'; _ -> False}) `any` sat
            then S.singleton $ SelfDependentError constrName n
            else S.empty
      in unsatisfied `S.union` selfDependent
    expand n = case _expand n of
      MidNode n' -> FixNode n'
      MidSplice s' -> FixNode $ SyntaxSplice s'
      m' -> error $ "Compiler error: expansion returned non-node: " ++ show m' ++ " in constrName: " ++ show constrName
    pairs = (id &&& expand) . mkNode <$> (traceVariationCount $ generateVariations pats)
    traceVariationCount pats = trace (constrName ++ ": " ++ show (length pats)) pats
    mkNode children = FixNode Node { name = constrName, children, nodeRange = mempty }

checkTypes :: M.Map String String -> M.Map String (Construction n) -> Node FakeNodeS -> Node FakeNodeS -> S.Set Error
checkTypes typeMap constrMap (FixNode p@Node{name}) (FixNode n) =
  recur mempty (MidNode n) (SyntaxPat $ constrTy name)
    & S.mapMonotonic (uncurry $ TypeError name)
  where
    recur r (MidNode n@Node{name, children}) (SyntaxPat ty) =
      S.unions $ err : cont
      where
        cont = zipWith (recur $ range n) children $
          case M.lookup name constrMap of
            Nothing -> error $ "Unknown construction " ++ name
            Just Construction{syntax} -> syntax
        gotTy = constrTy name
        expectedTy = baseTy ty
        err = if gotTy /= expectedTy
          then S.singleton $ (,) (gotTy, range n) (expectedTy, r)
          else S.empty
    recur r m@(MidNode (SyntaxSplice (FakeNodeS (FakeNode (SyntaxKind ty) _)))) (SyntaxPat ty') =
      if gotTy /= expectedTy
        then S.singleton $ (,) (gotTy, range m) (expectedTy, r)
        else S.empty
      where
        gotTy = baseTy ty
        expectedTy = baseTy ty'
    recur r m@(MidSplice (FakeNodeS (FakeNode (SyntaxKind ty) _))) (SyntaxPat ty') =
      if gotTy /= expectedTy
        then S.singleton $ (,) (gotTy, range m) (expectedTy, r)
        else S.empty
      where
        gotTy = baseTy ty
        expectedTy = baseTy ty'
    recur _ m@(Repeated _ cs) (RepeatPat p _) = S.unions .
      zipWith (recur $ range m) cs $ repeat p
    recur _ m@(Sequenced _ cs) (SequencePat ps) = S.unions $
      zipWith (recur $ range m) cs ps
    recur r m (NamedPat _ p) = recur r m p
    recur _ _ _ = S.empty
    constrTy :: String -> String
    constrTy constrName = case M.lookup constrName constrMap of
      Nothing -> error $ "Unknown construction " ++ constrName
      Just Construction{syntaxType} -> baseTy syntaxType
    baseTy :: String -> String
    baseTy ty = case M.lookup ty typeMap of
      Nothing -> error $ "Construction used unknown syntax type " ++ ty ++ "\n" ++ show n
      Just ty' -> ty'

mkTypeMap :: [(String, Maybe String)] -> M.Map String String
mkTypeMap tys = case foldlM step start higher of
  Left ty -> error $ "Unknown syntax type " ++ ty ++ " used in syntax type declaration"
  Right m -> m
  where
    separate (ty, Nothing) = Left (ty, ty)
    separate (ty, Just ty') = Right (ty, ty')
    (builtin, higher) = partitionEithers $ separate <$> tys
    start = M.fromList builtin
    step tys (ty, ty') = case M.lookup ty' tys of
      Nothing -> Left ty'
      Just ty'' -> Right $ M.insert ty ty'' tys

generateVariations :: [SyntaxPattern] -> [[MidNode FakeNodeS]]
generateVariations pats = zipWithM gen pats (prepend [])

type VariationM a = [a]

gen :: SyntaxPattern -> [Int] -> VariationM (MidNode FakeNodeS)
gen IdentifierPat{} path = return $ fakeIdentifier path
gen (SyntaxPat ty) path = return $ fakeNode (SyntaxKind ty) path
gen (RepeatPat pat rep) path = do
  reps <- repNum rep
  children <- sequence . take reps $ gen pat <$> (prepend path)
  return $ Repeated rep children
  where
    repNum StarRep = [0..2]
    repNum PlusRep = [1..3]
    repNum QuestionRep = [0..1]
gen (SequencePat pats) path = Sequenced mempty <$> zipWithM gen pats (prepend path)
gen (NamedPat _ pat) path = gen pat path
gen _ path = return $ fakeNode OtherKind path

fakeNode :: FakeKind -> [Int] -> MidNode FakeNodeS
fakeNode kind path = MidSplice . FakeNodeS $ FakeNode kind path

-- BUG: if a construction contains both defining and referencing identifier expansion may move them without preserving the reference, since all identifiers are generated with distinct symbols. Should instead be that all *binding* identifiers are generated with distinct symbols and all *referencing* identifiers are generated with FakeNodes that do not export any symbols, only refer to them, and all those dependencies must be preserved
fakeIdentifier :: [Int] -> MidNode FakeNodeS
fakeIdentifier path = MidIdentifier mempty $ GenSym (show path) 1

prepend :: [Int] -> [[Int]]
prepend path = (:path) <$> [0..]

data Direction = After | Before deriving (Show, Eq, Ord)
data Dependency = IdentifierDependency GenSym
                | SyntaxDependency FakeNode Direction
                deriving (Show, Eq, Ord)
data Export = SyntaxExport Direction FakeNode
            | IdentifierExport Direction GenSym
            deriving (Show, Eq, Ord)

newtype MonoidMap k v = MonoidMap { unMonoidMap :: M.Map k v }

instance (Ord k, Semigroup v) => Semigroup (MonoidMap k v) where
  MonoidMap a <> MonoidMap b = MonoidMap $ M.unionWith (<>) a b
instance (Ord k, Semigroup v) => Monoid (MonoidMap k v) where
  mempty = MonoidMap M.empty
  mappend = (<>)

computeDependencies :: (Applicative f, Semigroup (f (S.Set Dependency)), Monoid (f (S.Set Dependency)))
                    => M.Map String ResolvedConstruction
                    -> Node FakeNodeS
                    -> (S.Set Binding.Error, (M.Map FakeNode (f (S.Set Dependency)), S.Set Export))
computeDependencies _ (FixNode (SyntaxSplice (FakeNodeS s))) =
  (S.empty, (M.singleton s mempty, S.fromList [SyntaxExport Before s, SyntaxExport After s]))
computeDependencies resConstrs n =
  toErrors *** toExports $ Binding.resolveNamesS resSplice resConstrs n
  where
    toErrors (Data _) = S.empty
    toErrors (Error es) = S.fromList es
    toExports Binding.SpliceBindings{..} = (unMonoidMap result, before `S.union` after)
      where
        before = S.mapMonotonic (IdentifierExport Before) beforeBinding `S.union` S.map depToExport beforeSplice
        after = S.mapMonotonic (IdentifierExport After) afterBinding `S.union` S.map depToExport afterSplice
        depToExport (SyntaxDependency n dir) = SyntaxExport dir n
        depToExport dep = error $ "Compiler error: expected SyntaxDependency, got: " ++ show dep
    resSplice symbols (beforeSplice, afterSplice) (FakeNodeS n) =
      (newBindings, (FakeNodeS n, result))
      where
        newBindings = case n of
          FakeNode SyntaxKind{} _ -> (toDep Before, toDep After)
          _ -> (S.empty, S.empty)
        toDep = S.singleton . SyntaxDependency n
        result = MonoidMap . M.singleton n . pure $ S.unions [beforeSplice, afterSplice, S.mapMonotonic IdentifierDependency symbols]