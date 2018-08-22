module FullExpander (fullExpansion) where

import qualified Unsafe.Coerce

import Control.Arrow (second, (>>>))

import Data.Maybe (fromMaybe)
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Semigroup (Semigroup, (<>))
import qualified Data.Map as M
import qualified Data.Set as S

import Binding

import Types.Result
import Types.Paths (TreePath, here, mergePaths, step, oneOf, asTreePath)
import Types.Construction(NoSplice)
import Types.ResolvedConstruction
import Types.GenSym
import Types.Ast

type FNode = FixNode NoSplice GenSym
type Node = NodeI (NoSplice FNode) GenSym
type MidNode = MidNodeI (NoSplice FNode) GenSym

type Splice = (S.Set GenSym, FixNode ((,) (S.Set GenSym)) GenSym)
type NodeS = NodeI Splice GenSym
type MidNodeS = MidNodeI Splice GenSym

data Progress d n = Done d
                  | NeedsResolution n

data ContainsDuplicates = ContainsSingles (S.Set GenSym)
                        | ContainsDuplicates

instance Semigroup ContainsDuplicates where
  ContainsDuplicates <> _ = ContainsDuplicates
  _ <> ContainsDuplicates = ContainsDuplicates
  ContainsSingles a <> ContainsSingles b
    | S.null (S.intersection a b) = ContainsSingles $ a <> b
    | otherwise = ContainsDuplicates

pmap :: (d -> d') -> (n -> n') -> Progress d n -> Progress d' n'
pmap f _ (Done d) = Done $ f d
pmap _ f (NeedsResolution n) = NeedsResolution $ f n

fullExpansion :: M.Map String ResolvedConstruction -> FNode -> FNode
fullExpansion constructions = unSplice >>> typeChange >>> recur >>> FixNode
  where
    recur :: NodeS -> Node
    recur = expandNode >>> \case
      Done (_, n) -> removeSet n
      NeedsResolution n -> case resolveNames constructions (FixNode n) of
        Data n -> unSplice n & typeChange & recur
        Error es -> error $ "Name resolution error(s) during expansion, tree: " ++ prettyShow (FixNode n) ++ "\nerrors:\n" ++ show es
    expandNode :: NodeS -> Progress (S.Set GenSym, NodeS) Node
    expandNode s@(SyntaxSplice (binders, _)) = Done (binders, s)
    expandNode n@Node{name, children} = bindingPaths rc
      & expandMids children
      & \case
        Done (binders, cs) -> expand rc <*> pure (FixNode $ n { children = cs })
          & fmap (\case MidNode n -> n; _ -> error "Compiler error: expanded to non-node")
          & fmap expandNode
          & fromMaybe (Done (binders, n { children = cs }))
        NeedsResolution cs -> NeedsResolution $ n { children = cs }
      where
        rc = M.lookup name constructions & err ("Compiler error: unknown syntax construction \"" ++ name ++ "\"")
    expandMids :: [MidNodeS] -> TreePath -> Progress (S.Set GenSym, [MidNodeS]) [MidNode]
    expandMids cs p = cs
      & zipWith expandMid (step p <$> [0..])
      & listProgress
    expandMid :: TreePath -> MidNodeS -> Progress (S.Set GenSym, MidNodeS) MidNode
    expandMid _ (MidNode n) = expandNode n & pmap (second MidNode) MidNode
    expandMid p m@(MidIdentifier _ i)
      | (here :: TreePath) `oneOf` p = Done (S.singleton i, m)
      | otherwise = Done (S.empty, m)
    expandMid _ s@(MidSplice (binders, _)) = Done (binders, s)
    expandMid _ (Basic t) = Done $ (S.empty, Basic t)
    expandMid p (Repeated rep cs) = expandMids cs p & pmap (second $ Repeated rep) (Repeated rep)
    expandMid p (Sequenced r cs) = expandMids cs p & pmap (second $ Sequenced r) (Sequenced r)
    listProgress :: [Progress (S.Set GenSym, MidNodeS) MidNode] -> Progress (S.Set GenSym, [MidNodeS]) [MidNode]
    listProgress = foldr func (Done (S.empty, []))
      where
        func (NeedsResolution l) (NeedsResolution r) = NeedsResolution $ l : r
        func (NeedsResolution l) (Done (_, r)) = NeedsResolution $ l : (mRemoveSet <$> r)
        func (Done (_, l)) (NeedsResolution r) = NeedsResolution $ mRemoveSet l : r
        func (Done (lb, l)) (Done (rb, r))
          | S.null (S.intersection lb rb) = Done (lb <> rb, l : r)
          | otherwise = NeedsResolution $ mRemoveSet l : (mRemoveSet <$> r)
    bindingPaths :: ResolvedConstruction -> TreePath
    bindingPaths ResolvedConstruction{beforeBindings, afterBindings, inBindings} = inBindings
      & fmap (asTreePath . fst)
      & fold
      & mergePaths beforeBindings
      & mergePaths afterBindings
    err :: String -> Maybe a -> a
    err _ (Just a) = a
    err mess Nothing = error mess
    -- Since NoSplice is empty the input node has no splices, which makes this ok
    typeChange :: Node -> NodeS
    typeChange = Unsafe.Coerce.unsafeCoerce

removeSet :: NodeS -> Node
removeSet (SyntaxSplice (_, n)) = removeSet $ unSplice n
removeSet n@Node{children} = n {children = mRemoveSet <$> children}

mRemoveSet :: MidNodeS -> MidNode
mRemoveSet (MidNode n) = MidNode $ removeSet n
mRemoveSet (MidIdentifier r i) = MidIdentifier r i
mRemoveSet (MidSplice (_, m)) = MidNode . removeSet $ unSplice m
mRemoveSet (Basic t) = Basic t
mRemoveSet (Repeated rep ms) = Repeated rep $ mRemoveSet <$> ms
mRemoveSet (Sequenced r ms) = Sequenced r $ mRemoveSet <$> ms

-- data Progress = Done | NeedsResolution
--               deriving (Show, Eq)
-- instance Semigroup Progress where
--   NeedsResolution <> _ = NeedsResolution
--   _ <> NeedsResolution = NeedsResolution
--   Done <> Done = Done
-- instance Monoid Progress where
--   mempty = Done
--   mappend = (<>)


-- fullExpansion :: M.Map String ResolvedConstruction -> Node -> Node
-- fullExpansion constructions = recur
--   where
--     recur :: Node -> Node
--     recur n = case expandNode n of
--       (Done, MidNode expanded) -> FixNode expanded
--       (NeedsResolution, MidNode expanded) -> case resolveNames constructions $ FixNode expanded of
--         Data resolved -> recur resolved
--         Error es ->  error $ "Name resolution error(s) during expansion, tree: " ++ prettyShow (FixNode expanded) ++ "\nerrors:\n" ++ show es
--       (_, m) -> error $ "Compiler error: Somehow expanded to a midnode: " ++ show m
--     expandMid :: MidNode -> (Progress, MidNode)
--     expandMid m = undefined
--     expandNode :: Node -> (Progress, MidNode)
--     expandNode n@(FixNode Node{name}) = (M.lookup name constructions <|> err name >>= expand) <*> pure n
--       & \case
--       Nothing -> (Done, MidNode $ unSplice n)
--       Just (m, NoDuplication) -> expandMid m
--       Just (m, MightHaveDuplication) -> (NeedsResolution, m)
--     err name = error $ "Compiler error: Unknown construction: " ++ show name ++ ", constructions keys: " ++ show (M.keys constructions)

-- fullExpansion :: M.Map String ResolvedConstruction -> Node -> Node
-- fullExpansion constructions = recur
--   where
--     recur prev@(FixNode n) = case expandOne expander $ MidNode n of
--       Nothing -> prev
--       Just (MidNode expanded) -> case resolveNames constructions $ FixNode expanded of
--         Data resolved -> recur resolved
--         Error es -> error $ "Name resolution error(s) during expansion, tree: " ++ prettyShow (FixNode expanded) ++ "\nerrors:\n" ++ show es
--       Just m -> error $ "Compiler error: Somehow expanded to a midnode: " ++ show m
--     expander name n = (M.lookup name constructions <|> err name >>= expand) <*> pure n
--     err name = error $ "Compiler error: Unknown construction: " ++ show name ++ ", constructions keys: " ++ show (M.keys constructions)

-- -- Gives Nothing if there is nothing to expand, i.e. everything left is builtin
-- expandOne :: (String -> Node -> Maybe (MidNode, MightHaveDuplication)) -> MidNode -> Maybe MidNode
-- expandOne expander = \case
--   MidNode n@Node{name, children} -> (\ms -> MidNode $ n {children = ms}) <$> expandChild children
--                                 -- <|> trace name <$> expander name (FixNode n)
--                                 <|> expander name (FixNode n)
--   Repeated rep ms -> Repeated rep <$> expandChild ms
--   Sequenced r ms -> Sequenced r <$> expandChild ms
--   _ -> Nothing
--   where
--     expandChild :: [MidNode] -> Maybe [MidNode]
--     expandChild [] = Nothing
--     expandChild (m:ms) = (:ms) <$> expandOne expander m <|> (m:) <$> expandChild ms
