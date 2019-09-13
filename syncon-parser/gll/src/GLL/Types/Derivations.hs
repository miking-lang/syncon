{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE StandaloneDeriving #-}

module GLL.Types.Derivations where

import Prelude

import qualified    Data.Map as M
import qualified    Data.IntMap as IM
import qualified    Data.Set as S
import qualified    Data.IntSet as IS
import              Data.List (findIndices, partition, tails)
import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Arrow ((>>>), (&&&))
import Data.Foldable (fold)
import GLL.Types.Grammar

-- make sure that tokens are equal independent of their character level value
type SlotL t    = (Slot t, Int)                   -- slot with left extent
type PrL t      = (Prod t, Int)                     -- Production rule with left extent
type NtL        = (Nt, Int)                     -- Nonterminal with left extent

-- SPPF

-- |
-- An 'SPPF' contains symbol nodes, intermediate nodes, packed nodes and edges between them.
-- See Scott and Johnstone (2013) for an explanation of the 'SPPF'.
type SPPF t     =   (SymbMap t, ImdMap t, PackMap t, EdgeMap t)

-- |
-- Stores packed nodes using nested "Data.IntMap"s, nesting is as follows:
--
-- * left extent
-- * right extent
-- * dot position (from left to right)
-- * mapping from productions to set of pivots
type PackMap t  =   IM.IntMap (IM.IntMap (IM.IntMap (M.Map (Prod t) IS.IntSet)))

-- |
-- Stores symbol nodes using nested "Data.IntMap"s, nesting is as follows:
--
-- * left extent
-- * right extent
-- * set of symbols
type SymbMap t  =   IM.IntMap (IM.IntMap (S.Set (Symbol t)))

-- |
-- Stores intermediate nodes using nested "Data.IntMap"s, nesting is as follows:
--
-- * left extent
-- * right extent
-- * set of slots
type ImdMap t   =   IM.IntMap (IM.IntMap (S.Set (Slot t)))

-- |
-- Stores edges, potentially costly.
type EdgeMap t  =   M.Map (SPPFNode t) (S.Set (SPPFNode t))

-- |
-- An "SPPFNode" is either a symbol node, an intermediate node, a packed node or a dummy.
data SPPFNode t =   SNode (Symbol t, Int, Int)
                |   INode (Slot t, Int, Int)
                |   PNode (Slot t, Int, Int, Int)
                |   Dummy
    deriving (Ord, Eq)

type SNode t    = (Symbol t, Int, Int)
type PNode t    = (Prod t, [Int])
type SEdge t    = M.Map (SNode t)(S.Set (PNode t))
type PEdge t    = M.Map (PNode t) (S.Set (SNode t))

emptySPPF :: SPPF t
emptySPPF = (IM.empty, IM.empty, IM.empty, M.empty)

pNodeLookup :: (Ord t) => SPPF t -> ((Prod t, Int), Int, Int) -> Maybe [Int]
pNodeLookup (_,_,pMap,_) ((alt,j),l,r) = maybe Nothing inner $ IM.lookup l pMap
    where   inner   = maybe Nothing inner2 . IM.lookup r
            inner2  = maybe Nothing inner3 . IM.lookup j
            inner3  = maybe Nothing (Just . IS.toList) . M.lookup alt

pMapInsert :: (Ord t) => SPPFNode t -> SPPFNode t -> SPPF t -> SPPF t
pMapInsert f _ (sMap,iMap,pMap,eMap) =
    let pMap' = case f of
                    PNode ((Slot x alpha beta), l, k, r) ->
                        add (Prod x (alpha++beta)) (length alpha) l r k
                    _   -> pMap
    in (sMap,iMap,pMap',eMap)
 where add alt j l r k = IM.alter addInnerL l pMap
        where addInnerL mm = case mm of
                             Nothing -> Just singleRJAK
                             Just m ->  Just $ IM.alter addInnerR r m
              addInnerR mm = case mm of
                             Nothing -> Just singleJAK
                             Just m  -> Just $ IM.alter addInnerJ j m
              addInnerJ mm = case mm of
                             Nothing -> Just singleAK
                             Just m  -> Just $ M.insertWith IS.union alt singleK m
              singleRJAK= IM.fromList [(r, singleJAK)]
              singleJAK = IM.fromList [(j, singleAK)]
              singleAK  = M.fromList [(alt, singleK)]
              singleK   = IS.singleton k


sNodeLookup :: (Ord t) => SPPF t -> (Symbol t, Int, Int) -> Bool
sNodeLookup (sm,_,_,_) (s,l,r) = maybe False inner $ IM.lookup l sm
    where   inner   = maybe False (S.member s) . IM.lookup r

sNodeInsert :: (Ord t) => SPPFNode t -> SPPFNode t -> SPPF t -> SPPF t
sNodeInsert f t (sMap,iMap,pMap,eMap) =
    let sMap' = case f of
                SNode (s, l, r) -> newt (add s l r sMap)
                _               -> newt sMap
    in (sMap',iMap,pMap,eMap)
 where newt sMap = case t of
                   (SNode (s, l, r)) -> add s l r sMap
                   _                 -> sMap
       add s l r sMap = IM.alter addInnerL l sMap
        where addInnerL mm = case mm of
                             Nothing -> Just singleRS
                             Just m  -> Just $ IM.insertWith (S.union) r singleS m
              singleRS     = IM.fromList [(r, singleS)]
              singleS      = S.singleton s

sNodeRemove :: (Ord t) => SPPF t -> (Symbol t, Int, Int) -> SPPF t
sNodeRemove (sm,iMap,pMap,eMap) (s,l,r) =
    (IM.adjust inner l sm, iMap,pMap,eMap)
    where   inner   = IM.adjust ((s `S.delete`)) r

iNodeLookup :: (Ord t) => SPPF t -> (Slot t, Int, Int) -> Bool
iNodeLookup (_,iMap,_,_) (s,l,r) = maybe False inner $ IM.lookup l iMap
    where   inner   = maybe False (S.member s) . IM.lookup r

iNodeInsert :: (Ord t) => SPPFNode t -> SPPFNode t -> SPPF t -> SPPF t
iNodeInsert f t (sMap,iMap,pMap,eMap) =
    let iMap' = case f of
                INode (s, l, r) -> newt (add s l r iMap)
                _               -> newt iMap
    in (sMap,iMap',pMap,eMap)
 where newt iMap = case t of
                   (INode (s, l, r)) -> add s l r iMap
                   _                 -> iMap
       add s l r iMap = IM.alter addInnerL l iMap
        where addInnerL mm = case mm of
                             Nothing -> Just singleRS
                             Just m  -> Just $ IM.insertWith (S.union) r singleS m
              singleRS     = IM.fromList [(r, singleS)]
              singleS      = S.singleton s

iNodeRemove :: (Ord t) => SPPF t -> (Slot t, Int, Int) -> SPPF t
iNodeRemove (sMap,iMap,pMap,eMap) (s,l,r) =
    (sMap,IM.adjust inner l iMap,pMap,eMap)
    where   inner   = IM.adjust ((s `S.delete`)) r

eMapInsert :: (Ord t) => SPPFNode t -> SPPFNode t -> SPPF t -> SPPF t
eMapInsert f t (sMap,iMap,pMap,eMap) =
    (sMap,iMap,pMap,M.insertWith (S.union) f (S.singleton t) eMap)

-- helpers for Ucal
inU :: Ord a => (a, IS.Key, IS.Key) -> IM.IntMap (IM.IntMap (S.Set a)) -> Bool
inU (slot,l,i) u = maybe False inner $ IM.lookup l u
         where inner = maybe False (S.member slot) . IM.lookup i

toU :: Ord a => (a, IS.Key, IS.Key) -> IM.IntMap (IM.IntMap (S.Set a)) -> IM.IntMap (IM.IntMap (S.Set a))
toU (slot,l,i) u = IM.alter inner l u
 where inner mm = case mm of
                Nothing -> Just $ singleIS
                Just m  -> Just $ IM.insertWith S.union i singleS m
       singleIS = IM.fromList [(i,singleS)]
       singleS  = S.singleton slot


showD :: (Show a1, Show a2) => M.Map a1 [a2] -> String
showD dv = unlines [ show f ++ " --> " ++ show t | (f,ts) <- M.toList dv, t <- ts ]
showG :: (Show a1, Show a2) => M.Map a1 [a2] -> String
showG dv = unlines [ show f ++ " --> " ++ show t | (f,ts) <- M.toList dv, t <- ts ]
showP :: (Show a1, Show a2) => IM.IntMap (IM.IntMap (IM.IntMap (M.Map a1 a2))) -> String
showP pMap = unlines [ show ((a,j),l,r) ++ " --> " ++ show kset
                            | (l,r2j) <- IM.assocs pMap, (r,j2a) <- IM.assocs r2j
                            , (j,a2k) <- IM.assocs j2a, (a,kset) <- M.assocs a2k ]
showS :: Show a => IM.IntMap (IM.IntMap a) -> String
showS sMap = unlines [ show (l,r) ++ " --> " ++ show (sset)
                            | (l,r2s) <- IM.assocs sMap, (r,sset) <- IM.assocs r2s]

showSPPF :: Show t => SPPF t -> String
showSPPF (_,_,pMap,_) = showP pMap

type ProdMap t   = M.Map Nt [Prod t]
type PrefixMap t = M.Map (Prod t,Int) ([t], Maybe Nt)
type SelectMap t = M.Map (Nt, [Symbol t]) (S.Set t)
type FirstMap  t = M.Map Nt (S.Set t)
type FollowMap t = M.Map Nt (S.Set t)

fixedMaps :: forall t. (Ord t, Parseable t) => Nt -> [Prod t] ->
                (ProdMap t, PrefixMap t, FirstMap t, FollowMap t, SelectMap t)
fixedMaps s prs = (prodMap, prefixMap, firstMap, followMap, selectMap)
 where
    prodMap = M.fromListWith (++) [ (x,[pr]) | pr@(Prod x _) <- prs ]

    ensureComplete :: Monoid m => M.Map Nt m -> M.Map Nt m
    ensureComplete = M.unionWith (<>) $ prodMap <&> const mempty

    prefixMap = M.fromList
        [ ((pr,j), (tokens,msymb)) | pr@(Prod x alpha) <- prs
                                   , (j,tokens,msymb) <- prefix x alpha ]
     where
        prefix _ alpha = map rangePrefix ranges
         where  js          = (map ((+) 1) (findIndices isNt alpha))
                ranges      = zip (0:js) (js ++ [length alpha])
                rangePrefix (a,z) | a >= z = (a,[],Nothing)
                rangePrefix (a,z) =
                    let init = map (fromTerm . (alpha !!)) [a .. (z-2)]
                        fromTerm (Term t) = t
                        fromTerm _ = error "Not actually a term"
                        last = alpha !! (z-1)
                     in case last of
                           Nt nt     -> (a,init, Just nt)
                           Term t    -> (a,init ++ [t], Nothing)

    stepTransitions trans = trans <&> (\dests -> M.restrictKeys trans dests & fold & S.union dests)
    closure f prev = let next = f prev in if prev == next then prev else closure f next

    selectMap = M.fromList [ ((x,alpha), select alpha x) | Prod x rhs <- prs
                           , alpha <- tails rhs ]
     where
        -- TODO store intermediate results
        select alpha x      = res
                where   firsts  = firstRhs alpha
                        res     | eps `S.member` firsts     = S.delete eps firsts `S.union` (followMap M.! x)
                                | otherwise                 = firsts

    followMap :: M.Map Nt (S.Set t)
    followMap = [(nt2, S.singleton nt) | Prod nt syms <- prs, nt2 <- S.toList $ maybeFirstNts $ reverse syms]
      & M.fromListWith S.union
      & M.unionWith S.union (M.mapWithKey (\k _ -> S.singleton k) prodMap)
      & closure stepTransitions
      <&> M.restrictKeys initial
      <&> fold
      where
        initial = prs >>= (getSyms >>> tails)
          <&> mkFollows
          & M.unionsWith S.union
          & M.unionWith S.union (M.singleton s $ S.singleton eos)
        getSyms (Prod _ syms) = syms
        mkFollows [] = M.empty
        mkFollows (Term _ : _) = M.empty
        mkFollows (Nt nt : syms) = firstRhs syms & S.delete eps & M.singleton nt
        maybeFirstNts [] = S.empty
        maybeFirstNts (Term t : syms)
          | t == eps = maybeFirstNts syms
          | otherwise = S.empty
        maybeFirstNts (Nt nt : syms)
          | nt `S.member` nullableSet = S.insert nt $ maybeFirstNts syms
          | otherwise = S.singleton nt

    firstRhs :: [Symbol t] -> S.Set t
    firstRhs [] = S.singleton eps
    firstRhs (Term t : syms)
      | t == eps = firstRhs syms
      | otherwise = S.singleton t
    firstRhs (Nt nt : syms)
      | nt `S.member` nullableSet = M.lookup nt firstMap & fold & S.delete eps & S.union (firstRhs syms)
      | otherwise = M.lookup nt firstMap & fold

    firstMap :: M.Map Nt (S.Set t)
    firstMap = closure stepTransitions transitions
      <&> M.restrictKeys initial
      <&> fold
      & M.unionWith S.union (M.fromSet (const $ S.singleton eps) nullableSet)
      & ensureComplete
      where
        (transitions, initial) = prodMap <&> foldMap (\(Prod nt syms) -> (S.singleton nt, mempty) <> firstTerm syms) & (fmap fst &&& fmap snd)
        firstTerm [] = mempty
        firstTerm (Term t : syms)
          | t == eps = firstTerm syms
          | otherwise = (S.empty, S.singleton t)
        firstTerm (Nt nt : syms)
          | nt `S.member` nullableSet = (S.singleton nt, S.empty) <> firstTerm syms
          | otherwise = (S.singleton nt, S.empty)

    isNullableRhs :: S.Set Nt -> [Symbol t] -> Bool
    isNullableRhs nullableNts rhs = all isNullable rhs
      where
        isNullable (Term t) = t == eps
        isNullable (Nt nt) = nt `S.member` nullableNts

    -- NOTE: the original implementation did not compute nullable as normally considered, A -> BB would not be nullable, even if B is nullable
    nullableSet :: S.Set Nt
    nullableSet = prs
      <&> (\(Prod nt syms) -> (nt, syms))
      & filter (snd >>> all maybeNullable)
      & findNullables S.empty
      where
        maybeNullable (Term t) = t == eps
        maybeNullable (Nt _) = True

    findNullables :: S.Set Nt -> [(Nt, [Symbol t])] -> S.Set Nt
    findNullables prev prods
      | prev == next = prev
      | otherwise = findNullables next nextProds
      where
        (nullProds, nextProds) = partition (snd >>> isNullableRhs prev) prods
        next = nullProds <&> fst & S.fromList & S.union prev

{-
instance Show Symbol where
    show (Nt nt) = "Nt " ++ show nt
    show (Term t) = "Term " ++ show t
    show (Error t1 t2) = "Error " ++ show t1 ++ " " ++ show t2

instance Eq Symbol where
    (Nt nt) == (Nt nt') = nt == nt'
    (Term t) == (Term t') = t == t'
    (Error t1 t2) == (Error t1' t2') = (t1,t2) == (t1',t2')

instance Ord Symbol where
    (Nt nt) `compare` (Nt nt') = nt `compare` nt
    (Nt _)  `compare`  _       = LT
    _  `compare`  (Nt _)       = GT
    (Term t) `compare` (Term t') = t `compare` t'
    (Term _) `compare` _         = LT
    _        `compare` (Term _)   = GT
    (Error t1 t2) `compare` (Error t1' t2') = (t1,t2) `compare` (t1',t2')
-}
