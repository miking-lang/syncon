module FullExpander (fullExpansion) where

import qualified Unsafe.Coerce
import Debug.Trace

import Control.Arrow (second, (>>>), (***))
import Control.Monad (zipWithM)
import Control.Monad.State.Strict (State, evalState, get, modify')

import Data.Function ((&))
import Data.Semigroup (Semigroup, (<>))
import qualified Data.Map as M
import qualified Data.Set as S

import Binding

import Types.Result
import Types.Paths (TreePath, here, mergePaths, step, oneOf)
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

data Duplicates = Duplicates Bool (S.Set GenSym)

instance Semigroup Duplicates where
  Duplicates d1 s1 <> Duplicates d2 s2 =
    Duplicates (d1 || d2 || not disjoint) (s1 <> s2)
    where
      disjoint = S.null $ S.intersection s1 s2
instance Monoid Duplicates where
  mempty = Duplicates False S.empty
  mappend = (<>)

pmap :: (d -> d') -> (n -> n') -> Progress d n -> Progress d' n'
pmap f _ (Done d) = Done $ f d
pmap _ f (NeedsResolution n) = NeedsResolution $ f n

type Expander a = State Int a

fullExpansion :: M.Map String ResolvedConstruction -> FNode -> FNode
fullExpansion constructions = unSplice >>> typeChange >>> recur >>> FixNode
  where
    recur :: NodeS -> Node
    recur n = case evalState (expandNode n) (-1) of
      Done (_, n) -> trace "done" $ removeSet n
      NeedsResolution n -> case resolveNames constructions (FixNode n) of
        Data n -> trace "recur" $ unSplice n & typeChange & recur
        Error es -> error $ "Name resolution error(s) during expansion, tree: " ++ prettyShow (FixNode n) ++ "\nerrors:\n" ++ show es
    err :: String -> Maybe a -> a
    err _ (Just a) = a
    err mess Nothing = error mess
    -- Since NoSplice is empty the input node has no splices, which makes this ok
    typeChange :: Node -> NodeS
    typeChange = Unsafe.Coerce.unsafeCoerce

    expandNode :: NodeS -> Expander (Progress (S.Set GenSym, NodeS) Node)
    expandNode s@(SyntaxSplice (binders, _)) = return $ Done (binders, s)
    expandNode n@Node{name, children} = do
      expandId <- get <* modify' (subtract 1)
      expandMids children (bindingPaths rc) >>= \case
        NeedsResolution cs -> return $ NeedsResolution $ n { children = cs }
        Done (Duplicates duped binders, cs) -> case expand rc of
          Nothing -> return $ Done (binders, n { children = cs })
          Just expand -> if duped
            then return $ NeedsResolution $ n { children = mRemoveSet <$> cs }
            else case expand expandId (FixNode $ n { children = cs }) of
              MidNode n -> expandNode n
              _ -> error $ "Compiler error: " ++ name ++ " expanded to non-node"
      where
        rc = M.lookup name constructions & err ("Compiler error: unknown syntax construction \"" ++ name ++ "\"")

    expandMids :: [MidNodeS] -> TreePath -> Expander (Progress (Duplicates, [MidNodeS]) [MidNode])
    expandMids cs p = cs
      & zipWithM expandMid (step p <$> [0..])
      & fmap listProgress

    expandMid :: TreePath -> MidNodeS -> Expander (Progress (Duplicates, MidNodeS) MidNode)
    expandMid _ (MidNode n) = pmap (Duplicates False *** MidNode) MidNode <$> expandNode n
    expandMid p m@(MidIdentifier _ i)
      | (here :: TreePath) `oneOf` p = return $ Done (Duplicates False $ S.singleton i, m)
      | otherwise = return $ Done (Duplicates False S.empty, m)
    expandMid _ s@(MidSplice (binders, _)) = return $ Done (Duplicates False binders, s)
    expandMid _ (Basic t) = return $ Done $ (Duplicates False S.empty, Basic t)
    expandMid p (Repeated rep cs) = pmap (second $ Repeated rep) (Repeated rep) <$> expandMids cs p
    expandMid p (Sequenced r cs) = pmap (second $ Sequenced r) (Sequenced r) <$> expandMids cs p

listProgress :: [Progress (Duplicates, MidNodeS) MidNode] -> Progress (Duplicates, [MidNodeS]) [MidNode]
listProgress = foldr func (Done (mempty, []))
  where
    func (NeedsResolution l) (NeedsResolution r) = NeedsResolution $ l : r
    func (NeedsResolution l) (Done (_, r)) = NeedsResolution $ l : (mRemoveSet <$> r)
    func (Done (_, l)) (NeedsResolution r) = NeedsResolution $ mRemoveSet l : r
    func (Done (ld, l)) (Done (rd, r)) = Done (ld <> rd, l : r)

-- NOTE: inBindings can never be exposed outside of a node, making it unnecessary to track them here
bindingPaths :: ResolvedConstruction -> TreePath
bindingPaths ResolvedConstruction{beforeBindings, afterBindings} = mergePaths beforeBindings afterBindings


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
