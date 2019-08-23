
module GLL.Combinators.Visit.Sem where

import Prelude

import GLL.Combinators.Options
import GLL.Types.Grammar
import GLL.Types.Derivations

import Control.Monad (forM)
import qualified Data.Array as A
import qualified Data.IntMap as IM
import qualified Data.Set as S

type Sem_Symb t a = PCOptions -> Ancestors t
                        -> SPPF t -> A.Array Int t -> Int -> Int -> IO [a]
type Sem_Alt  t a = PCOptions -> (Prod t,Int) -> Ancestors t
                        -> SPPF t -> A.Array Int t -> Int -> Int -> IO [(Int,a)]

sem_nterm :: Bool -> Bool -> Nt -> [Prod t] -> [Sem_Alt t a] -> Sem_Symb t a
sem_nterm use_ctx left_biased x alts ps opts ctx sppf arr l r =
        let ctx' = ctx `toAncestors` (x,l,r)
            sems = zip alts ps
            seq (alt@(Prod _ rhs), va3) =
                va3 opts (alt,length rhs) ctx' sppf arr l r
        in if use_ctx && ctx `inAncestors` (Nt x, l, r)
                then return []
                else do ass <- forM sems seq
                        let choices = case (pivot_select_nt opts, pivot_select opts) of
                                        (True,Just compare) -> maintainWith compare ass
                                        _                   -> ass
                        return (concatChoice left_biased opts (map (map snd) choices))
 where
    concatChoice :: Bool -> PCOptions -> [[a]] -> [a]
    concatChoice left_biased opts ress =
        if left_biased || left_biased_choice opts
        then firstRes ress
        else concat ress
     where  firstRes []         = []
            firstRes ([]:ress)  = firstRes ress
            firstRes (res:_)    = res

sem_apply :: Ord t => (a -> b) -> Sem_Symb t a -> Sem_Alt t b
sem_apply f p opts (alt,j) ctx sppf arr l r =
        let op f a = (r,f a)
        in do   as <- p opts ctx sppf arr l r
                return (maybe [] (const (map (op f) as)) $ sppf `pNodeLookup` ((alt,1),l,r))

sem_seq :: Ord t => CombinatorOptions -> Sem_Alt t (a -> b) -> Sem_Symb t a -> Sem_Alt t b
sem_seq local_opts p q opts (alt@(Prod x rhs),j) ctx sppf arr l r =
    let ks      = maybe [] id $ sppf `pNodeLookup` ((alt,j), l, r)
        choices = case pivot_select (runOptionsOn opts local_opts) of
                    Nothing      -> ks
                    Just compare -> maximumsWith compare ks
        seq k  = do     as      <- q opts ctx' sppf arr k r
                        a2bs    <- p opts (alt,j-1) ctx'' sppf arr l k
                        return [ (k,a2b a) | (_,a2b) <- a2bs, a <- as ]
          where ctx'  | k > l       = emptyAncestors
                      | otherwise   = ctx
                ctx'' | k < r       = emptyAncestors
                      | otherwise   = ctx
    in do   ass <- forM choices seq
            return (concat ass)

--- contexts
type Ancestors t = S.Set Nt

emptyAncestors :: Ancestors t
emptyAncestors = S.empty

inAncestors :: Ancestors t -> (Symbol t, Int, Int) -> Bool
inAncestors ctx (Term _, _, _) = False
inAncestors ctx (Nt x, l, r) = S.member x ctx

toAncestors :: Ancestors t -> (Nt, Int, Int) -> Ancestors t
toAncestors ctx (x, l, r) = S.insert x ctx
