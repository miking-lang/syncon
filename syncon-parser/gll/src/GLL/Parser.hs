
{-|
Implementation of the GLL parsing algorithm [Scott and Johnstone 2010,2013,2016]
with the grammar as an explicit parameter.

Function 'parse' receives a 'Grammar' as input together with a 
list of tokens (the input string).

The type of token is chosen arbitrarily, but the type should be 'Parseable' and 'Ord'erable.
To be 'Parseable' a type must have two distinct values, 'eos' (end-of-string)
and 'eps' (epsilon). The user must ensure that these two values will never occur
as part of the input string.

== GLL Parsing
=== Recursive Descent
GLL parsing is a generalisation of recursive descent parsing (RD parsing).
A RD parser (RDP), for some grammar 'G' , consists of a set of parse 
functions 'f_X', one for every nonterminal 'X', and a main function which 
calls 'f_S', where 'S' is the start symbol. 
The parse function 'f_X' take an integer 'l' as an argument and produces an 
integer 'r', indicating that nonterminal 'X' derives 's_l_r', 
where 's_i_j' is the substring of the input string 's' ranging from
'i' to 'j'. We call 'l' and 'r' the right- and left-extent, respectively.

The parse function 'f_X'
has a branch for every production X ::= s_1 ... s_k in 'G', guarded
by a look-ahead test, and every
branch has 'k' code fragments, one for every symbol 's_i', 
with 1 <= i <= k.
A RDP matches grammar positions, represented by /slots/ of the form
X ::= a . b,  with (input) string positions.
The dot in a slot tells us how much of the production's symbols have been 
matched (the symbols before the dot) and which symbols still need to 
be matched (the symbols after the dot). The symbol immediately after the dot
is the next symbol to be match and is either:

* A terminal token, matched directly with the token at the current
        string position.
* A nonterminal 'Y', for which 'f_Y' is called. In the case of
        LL(1) deterministic parsing, only one (or none) of the productions
        of 'Y' passes the lookahead-test, say "Y ::= c", and its branch 
        will be executed: the next grammar position is "Y ::= .c".
* No further symbol, represented by "X ::= d."  (all 
        symbols have been processed). In this case a return call is made
        to the caller of 'f_X' (relying on a function call stack).

=== Handling function/return calls
GLL handles its own function calls and return calls, instead of relying on an 
underlying mechanism. This form of low-level control allows
GLL to avoid much duplicate work, not only for function calls (as in classical
memoisation) but also for return calls. The underlying observation is that
both return calls and function calls continue matching grammar slots. 
In non-deterministic RDP, every function call leads to a slot of the
form "X ::= . a" being processed, while every return call 
leads to a slot of the form "X ::= aY.b" being processed,
where 'Y' is some nonterminal. GLL uses /descriptors/, containing
a slot of one of these forms, to uniquely identify the computation that
processes the slot. The descriptor therefore also needs to contain
the initial values of the local variables used in that computation. 

A generated GLL parser (Scott and Johnstone 2013) has a code fragment for 
every nonterminal 'X' (labelled 'L_X') and slot (labelled "L_{X ::= a.b}"). 
This Haskell implementation abstracts over the grammar and has a function for
executing 'L_X', for a given 'X', and a function for executing 
"L_{X ::= a.b}", for a given "X ::= a.b".

=== Generalisation
GLL parsing generalises RD parsing by allowing non-determinism:
when processing "X ::= a.Yb", all productions of 'Y', that pass 
the lookahead test, are considered. A slot is considered, by adding a 
descriptor for it to the /worklist/ 'R'. 
Duplicates in the worklist are avoided by maintaining a separate descriptor-set
'U' containing all descriptors added to the worklist before.

The result of a parse function 'f_X' is no longer a single right extent 'r'.
Instead, it is a list of right extents 'rs', indicating that 'X' derives
's_l_r' for all 'r' in 'rs' and integer input 'l' (left extent).
Every discovered right extent is stored in the /pop-set/ 'P'.

When a descriptors for a function call is a duplicate, it is not added to the
worklist, but we have to make sure that the corresponding
return call is still made. Note that a function call to 'f_Y', with 
the same parameters, can be made from multiple right-hand side occurrences
of 'Y'. It might be the case that:

* The original descriptors is still being processed. 
    Once finished, a descriptor must be added for all return calls 
    corresponding to function calls that lead to duplicates of 
    this descriptor. 
    GLL uses a Graph-Structured Stack (GSS) to efficiently maintain multiple 
    such continuations.
* The original descriptors has already been processed. In this
    case, one or more right extents 'rs' are stored in 'P' for the 
    corresponding function call. A descriptor for the return call must be 
    added for all 'r' in 'rs'. The descriptor for the return call must 
    be added to the GSS in this case as well, as other right extents might 
    be found in the future.
 

== Usage
This module provides generalised parsing to other applications that work with 
BNF grammars. 

The user should provide a 'Grammar' and an input string as arguments
to top-level functions 'parse' or 'parseWithOptions'.

=== Example
This example shows simple character level parsing.
First we must make 'Char' and instance of 'Parseable'.

@
instance Parseable Char where
    eos = \'$\'
    eps = '#'
@

This instance mandates that \'$\' and '#' are 'reserved tokens' 
and not part of the input string. This instance is available as an import: 
"GLL.Parseable.Char".

"GLL.Parser" exports smart constructors for constructing 'Grammar's.

@
grammar1 = (start \"X\" , [prod \"X\" [nterm \"A\", nterm \"A\"]
                      , prod \"A\" [term \'a\']
                      , prod \"A\" [term \'a\', term \'a\']
                 ] )

fail1       = "a"
success1    = "aa"
success2    = "aaa"
fail2       = "aaaaa"
@
Note that there are two possible derivations of 'success2'.

The parser can be accessed through 'parse' or 'parseWithOptions'.

@
run1 = parse grammar1 success1
run2 = parseWithOptions [fullSPPF, strictBinarisation] grammar1 success2
@   

The options 'fullSPPF', 'allNodes', 'packedNodesOnly', decide whether all SPPF nodes and 
edges are inserted into the resulting value of the 'SPPF' type.
Packed nodes are enough to fully represent an SPPF, as the parent and children
of a packed node can be computed from the packed nodes' information.
For efficiency the 'SPPF' is not strictly binarised by default: a packed
node might have a symbol node as a left child. In a strictly binarised 'SPPF'
a packed node has an intermediate node as a left child, or no left child at all.
To create a strictly binarised 'SPPF' (necessary for "GLL.Combinators") the option
'strictBinarisation' is available.

=== Combinator interface
Module "GLL.Combinators.Interface" provides a combinator interface to access
"GLL.Parser". Applicative-like combinators are used to specify a 'Grammar' and
call 'parse'. The 'SPPF' is then used to produce semantic results.

-}
module GLL.Parser (
        -- * Grammar
        Grammar(..), Prods(..), Prod(..), Symbols(..), Symbol(..), Slot(..), 
        -- ** Smart constructors for creating 'Grammar's
        start, prod, nterm, term,
        -- ** Parseable tokens 
        Parseable(..), Input, mkInput,
        -- * Run the GLL parser
        parse, parseArray,
        -- ** Run the GLL parser with options
        parseWithOptions, parseWithOptionsArray,
        -- *** ParseOptions
        ParseOptions, ParseOption, 
        strictBinarisation, fullSPPF, allNodes, packedNodesOnly, maximumErrors,
          noSelectTest,
        -- ** Result
        ParseResult(..), SPPF(..), SPPFNode(..), SymbMap, ImdMap, PackMap, EdgeMap, showSPPF,
    ) where

import Data.Foldable hiding (forM_, toList, sum)
import Prelude  hiding (lookup, foldr, fmap, foldl, elem, any, concatMap)
import Control.Applicative 
import Control.Monad
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Array as A
import qualified Data.Set as S
import qualified Data.IntSet as IS
import Data.Text (pack)
import Text.PrettyPrint.HughesPJ as PP

import GLL.Types.Grammar
import GLL.Types.Derivations
import GLL.Flags

-- | Create an 'Nt' (nonterminal) from a String.
string2nt :: String -> Nt
string2nt = pack

-- | A smart constructor for creating a start 'Nt' (nonterminal).
start :: String -> Nt
start = string2nt

-- | A smart constructor for creating a 'Prod' (production).
prod :: String -> Symbols t -> Prod t
prod x = Prod (string2nt x)

-- | A smart constructor for creating a nonterminal 'Symbol'.
nterm :: String -> Symbol t
nterm = Nt . string2nt

-- | A smart constructor for creating a terminal 'Symbol'.
term :: t -> Symbol t
term = Term

-- | Representation of the input string
type Input t        =   A.Array Int t 
mkInput :: (Parseable t) => [t] -> Input t
mkInput input = A.listArray (0,m) (input++[eos])
  where m = length input

-- | Types for 
type LhsParams t    =   (Nt, Int)
type RhsParams t    =   (Slot t, Int, Int)

-- | The worklist and descriptor set
type Rcal t         =   [(RhsParams t, SPPFNode t)]
type Ucal t         =   IM.IntMap (IM.IntMap (S.Set (Slot t)))

-- | GSS representation
type GSS t          =   IM.IntMap (M.Map Nt [GSSEdge t])
type GSSEdge t      =   (Slot t, Int, SPPFNode t) -- return position, left extent
type GSSNode t      =   (Nt, Int)

type MisMatches t   =   IM.IntMap (S.Set t)

-- | Pop-set
type Pcal t         =   IM.IntMap (M.Map Nt [Int])

-- | Connecting it all
data Mutable t      =   Mutable { mut_sppf          :: SPPF t
                                , mut_worklist      :: Rcal t
                                , mut_descriptors   :: Ucal t
                                , mut_gss           :: GSS t
                                , mut_popset        :: Pcal t 
                                , mut_mismatches    :: MisMatches t 
                                , mut_counters      :: Counters
                                }

data Counters = Counters  { count_successes :: Int
                          , count_pnodes    :: Int 
                          }

-- | Monad for implicitly passing around 'context'
data GLL t a        =   GLL (Flags -> Mutable t -> (a, Mutable t))

runGLL :: GLL t a -> Flags -> Mutable t -> Mutable t
runGLL (GLL f) o p = snd $ f o p

addSPPFEdge f t = GLL $ \flags mut -> 
    let sppf' = (if symbol_nodes flags          then sNodeInsert f t else id) $
                (if intermediate_nodes flags    then iNodeInsert f t else id) $
                (if edges flags                 then eMapInsert f t  else id) $ 
                    pMapInsert f t (mut_sppf mut)
    in ((),mut{mut_sppf = sppf'})

addDescr sppf alt@(slot,i,l) = GLL $ \_ mut -> 
    let new     = maybe True inner $ IM.lookup i (mut_descriptors mut)
          where inner m = maybe True (not . (slot `S.member`)) $ IM.lookup l m
        newU = IM.alter inner i (mut_descriptors mut)
         where inner mm = case mm of 
                             Nothing -> Just $ IM.singleton l single 
                             Just m  -> Just $ IM.insertWith (S.union) l single m
               single = S.singleton slot
     in if new then ((), mut{mut_worklist       = (alt,sppf):(mut_worklist mut)
                            ,mut_descriptors    = newU})
               else ((), mut)

getDescr = GLL $ \_ mut -> 
    case mut_worklist mut of 
        []                      -> (Nothing, mut)
        (next@(alt,sppf):rest)  -> (Just next, mut{mut_worklist = rest})

addPop (gs,l) i = GLL $ \_ mut ->
    let newP = IM.alter inner l (mut_popset mut)
         where inner mm = case mm of 
                            Nothing -> Just $ M.singleton gs [i]
                            Just m  -> Just $ M.insertWith (++) gs [i] m
    in ((), mut{mut_popset = newP})

getChildren (gs,l) = GLL $ \_ mut ->
    let res = maybe [] inner $ IM.lookup l (mut_gss mut)
         where inner m = maybe [] id $ M.lookup gs m
     in (res, mut)

addGSSEdge f@(gs,i) t = GLL $ \_ mut -> 
    let newGSS = IM.alter inner i (mut_gss mut)
         where inner mm = case mm of 
                            Nothing -> Just $ M.singleton gs [t] 
                            Just m  -> Just $ M.insertWith (++) gs [t] m
    in ((), mut{mut_gss = newGSS})

getPops (gs,l) = GLL $ \_ mut -> 
    let res = maybe [] inner $ IM.lookup l (mut_popset mut)
         where inner = maybe [] id .  M.lookup gs
    in (res, mut)

addSuccess = GLL $ \_ mut -> 
  let mut' = mut { mut_counters = counters { count_successes = 1 + count_successes counters } }
      counters = mut_counters mut
  in ((),mut')

getFlags = GLL $ \fs ctx -> (fs, ctx)

addMisMatch :: (Ord t) => Int -> S.Set t -> GLL t ()
addMisMatch k ts = GLL $ \flags mut -> 
    let newM    = IM.insertWith S.union k ts (mut_mismatches mut)
        newM'   | length (IM.keys newM) > max_errors flags = IM.deleteMin newM
                | otherwise                                = newM
    in ((), mut{mut_mismatches = newM'})

instance (Show t) => Show (SPPFNode t) where
    show (SNode (s, l, r))  = "(s: " ++ show s ++ ", " ++ show l ++ ", " ++ show r ++ ")"
    show (INode (s, l, r))  = "(i: " ++ show s ++ ", " ++ show l ++ ", " ++ show r ++ ")"
    show (PNode (p, l, k, r))  = "(p: " ++ show p ++ ", " ++ show l ++ ", " ++ show k ++ ", " ++ show r ++ ")"
    show Dummy              = "$"

instance Applicative (GLL t) where
    (<*>) = ap
    pure  = return
instance Functor (GLL t) where
    fmap  = liftM
instance Monad (GLL t) where
    return a = GLL $ \_ p -> (a, p)
    (GLL m) >>= f  = GLL $ \o p -> let (a, p')  = m o p
                                       (GLL m') = f a
                                    in m' o p'

-- | 
-- Run the GLL parser given a 'Grammar' 't' and a list of 't's, 
-- where 't' is an arbitrary token-type.
-- All token-types must be 'Parseable'.
parse :: (Parseable t) => Grammar t -> [t] -> ParseResult t
parse = parseWithOptions [] 

-- | 
-- Run the GLL parser given a 'Grammar' 't' and an 'Array' of 't's, 
-- where 't' is an arbitrary token-type.
-- All token-types must be 'Parseable'.
parseArray :: (Parseable t) => Grammar t -> Input t -> ParseResult t
parseArray = parseWithOptionsArray []

-- | 
-- Variant of 'parseWithOptionsArray' where the input is a list of 'Parseable's rather than an 'Array'
parseWithOptions :: Parseable t => ParseOptions -> Grammar t -> [t] -> ParseResult t
parseWithOptions opts gram  = parseWithOptionsArray opts gram . mkInput

-- | 
-- Run the GLL parser given some options, a 'Grammar' 't' and a list of 't's.
--
-- If no options are given a minimal 'SPPF' will be created:
--
--  * only packed nodes are created
--  * the resulting 'SPPF' is not strictly binarised
parseWithOptionsArray :: Parseable t => ParseOptions -> Grammar t -> Input t -> ParseResult t
parseWithOptionsArray opts grammar@(start,_) input = 
    let flags           = runOptions opts
        (mutable,_,_)   = gll flags m False grammar input
        (_, m)          = A.bounds input 
    in resultFromMutable input flags mutable (Nt start, 0, m)

gll :: Parseable t => Flags -> Int -> Bool -> Grammar t -> Input t -> 
            (Mutable t, SelectMap t, FollowMap t)
gll flags m debug (start, prods) input = 
    (runGLL (pLhs (start, 0)) flags context, selects, follows)
 where 
    context = Mutable emptySPPF [] IM.empty IM.empty IM.empty IM.empty counters
    counters = Counters 0 0

    dispatch = do
        mnext <- getDescr
        case mnext of
            Nothing            -> return () -- no continuation
            Just (next,sppf)   -> pRhs next sppf

    pLhs (bigx, i) = do 
        let     alts  =  [  ((Slot bigx [] beta, i, i), first_ts) 
                         | Prod bigx beta <- altsOf bigx
                         , let first_ts = select beta bigx 
                         ]
                first_ts = S.unions (map snd alts)
                cands = [ descr | (descr, first_ts) <- alts
                                , select_test (input A.! i) first_ts ]
        if null cands
            then addMisMatch i first_ts
            else forM_ cands (addDescr Dummy)
        dispatch 

    pRhs (Slot bigx alpha ((Term tau):beta), i, l) sppf = 
     if (input A.! i `matches` tau) 
      then do -- token test successful 
        root <-  joinSPPFs slot sppf l i (i+1) 
        pRhs (slot, i+1, l) root 
      else do
        addMisMatch i (S.singleton tau)
        dispatch
     where  slot       = Slot bigx (alpha++[Term tau]) beta

    pRhs (Slot bigx alpha ((Nt bigy):beta), i, l) sppf = 
      if select_test (input A.! i) first_ts
        then do
          addGSSEdge ret (slot,l,sppf)
          rs <- getPops ret     -- has ret been popped?
          forM_ rs $ \r -> do   -- yes, use given extents
                          root <- joinSPPFs slot sppf l i r
                          addDescr root (slot, r, l)
          pLhs (bigy, i)
        else do
          addMisMatch i first_ts
          dispatch
     where  ret      = (bigy, i)
            slot     = Slot bigx (alpha++[Nt bigy]) beta
            first_ts = select ((Nt bigy):beta) bigx 

    pRhs (Slot bigy alpha [], i, l) sppf | bigy == start && l == 0 = 
        if i == m 
          then addSuccess >> dispatch 
          else addMisMatch i (S.singleton eos) >> dispatch

    pRhs (Slot bigx alpha [], i, l) Dummy  = do
        root <- joinSPPFs slot Dummy l i i
        pRhs (slot, i, l) root
     where  slot    = Slot bigx [] []

    pRhs (Slot bigy alpha [], i, l) ynode = do
        addPop (bigy,l) i
        returns <- getChildren (bigy,l) 
        forM_ returns $ \(gs',l',sppf) -> do  
            root <- joinSPPFs gs' sppf l' l i  -- create SPPF for lhs
            addDescr root (gs', i, l')   -- add new descriptors
        dispatch

    (prodMap,_,_,follows,selects)   
        | do_select_test flags = fixedMaps start prods 
        | otherwise = (pmap, undefined, undefined, undefined, 
                         error "select-tests are switched off")
      where pmap = M.fromListWith (++) [ (x,[pr]) | pr@(Prod x _) <- prods ]
    follow x          = follows M.! x
    do_test = do_select_test flags     
    select rhs x      | do_test   = selects M.! (x,rhs)
                      | otherwise = S.empty 
      where 
    select_test t set | do_test   = any (matches t) set
                      | otherwise = True
    altsOf x          = prodMap M.! x
    merge m1 m2 = IM.unionWith inner m1 m2
     where inner  = IM.unionWith S.union 

count_pnode :: GLL t ()
count_pnode = GLL $ \flags mut -> 
    let mut' = mut { mut_counters = mut_counters' (mut_counters mut) }
          where mut_counters' counters = counters { count_pnodes = count_pnodes counters + 1 }
    in ((), mut')

joinSPPFs (Slot bigx alpha beta) sppf l k r = do
    flags <- getFlags
    case (flexible_binarisation flags, sppf, beta) of
        (True,Dummy, _:_) ->  return snode
        (_,Dummy, [])     ->  do  addSPPFEdge xnode pnode
                                  addSPPFEdge pnode snode
                                  count_pnode
                                  return xnode
        (_,_, [])         ->  do  addSPPFEdge xnode pnode
                                  addSPPFEdge pnode sppf
                                  addSPPFEdge pnode snode
                                  count_pnode
                                  return xnode
        _                 ->  do  addSPPFEdge inode pnode
                                  addSPPFEdge pnode sppf
                                  addSPPFEdge pnode snode
                                  count_pnode
                                  return inode
 where  x       =   last alpha  -- symbol before the dot
        snode   =   SNode (x, k, r)     
        xnode   =   SNode (Nt bigx, l, r)
        inode   =   INode ((Slot bigx alpha beta), l, r)
        pnode   =   PNode ((Slot bigx alpha beta), l, k, r)

-- | 
-- The "ParseResult" datatype contains the "SPPF" and some other 
--  information about the parse:
--
--  * 'SPPF'
--  * Whether the parse was successful
--  * The number of descriptors that have been processed
--  * The number of symbol nodes (nonterminal and terminal)
--  * The number of intermediate noes
--  * The number of packed nodes
--  * The number of GSS nodes
--  * The number of GSS edges
data ParseResult t = ParseResult{ sppf_result               :: SPPF t
                                , res_success               :: Bool
                                , res_successes             :: Int
                                , nr_descriptors            :: Int
                                , nr_nterm_nodes            :: Int
                                , nr_term_nodes             :: Int
                                , nr_intermediate_nodes     :: Int
                                , nr_packed_nodes           :: Int
                                , nr_packed_node_attempts   :: Int
                                , nr_sppf_edges             :: Int
                                , nr_gss_nodes              :: Int
                                , nr_gss_edges              :: Int
                                , error_message             :: String
                                }

resultFromMutable :: Parseable t => Input t -> Flags -> Mutable t -> SNode t -> ParseResult t
resultFromMutable inp flags mutable s_node@(s, l, m) =
    let u           = mut_descriptors mutable
        gss         = mut_gss mutable
        usize       = sum  [ S.size s   | (l, r2s) <- IM.assocs u
                                        , (r,s) <- IM.assocs r2s ]
        s_nodes     = sum [ S.size s    | (l, r2s) <- IM.assocs sMap
                                        , (r, s)   <- IM.assocs r2s ]
        i_nodes     = sum [ S.size s    | (l, r2s) <- IM.assocs iMap
                                        , (r, s)   <- IM.assocs r2s ]
        p_nodes     = sum [ IS.size ks  | (l, r2j) <- IM.assocs pMap
                                        , (r, j2s) <- IM.assocs r2j
                                        , (j, s2k) <- IM.assocs j2s
                                        , (s, ks)  <- M.assocs s2k ]
        sppf_edges  = sum [ S.size ts | (_, ts) <- M.assocs eMap ]
        gss_nodes   = 1 + sum [ length $ M.keys x2s| (l,x2s) <- IM.assocs gss] 
        gss_edges   = 1 + sum [ length s    | (l,x2s) <- IM.assocs gss
                                            , (x,s)   <- M.assocs x2s ]
        sppf@(sMap, iMap, pMap, eMap) = mut_sppf mutable
        successes = count_successes (mut_counters mutable)
    in ParseResult sppf (successes > 0) successes usize s_nodes m i_nodes p_nodes (count_pnodes (mut_counters mutable)) sppf_edges gss_nodes gss_edges (renderErrors inp flags (mut_mismatches mutable))

renderErrors :: Parseable t => Input t -> Flags -> MisMatches t -> String
renderErrors inp flags mm = render doc 
 where  n       = max_errors flags
        locs    = reverse (IM.assocs mm)
        doc     = text ("Unsuccessful parse, showing "++ show n ++ " furthest matches") $+$
                    foldr (\loc -> (ppLoc loc $+$)) PP.empty locs

        ppLoc (k, ts) = text ("did not match at position " ++ show k ++ ", where we find " ++ lexeme) $+$
                            nest 4 (text "Found" <+> ppExp token) $+$
                            nest 4 (text "expected:") $+$
                                nest 8 (vcat (map ppExp (S.toList ts)))
         where  token = inp A.! k
                lexeme = concatMap unlex (take 5 (drop k (A.elems inp)))
        ppExp t = text (unlex t) <+> text "AKA" <+> text (show t)

instance Show (ParseResult t) where
    show res | res_success res = result_string
             | otherwise       = result_string ++ "\n" ++ error_message res
     where result_string = unlines $
                [   "Success             "  ++ show (res_success res)
                ,   "#Success            "  ++ show (res_successes res)
                ,   "Descriptors:        "  ++ show (nr_descriptors res)
                ,   "Nonterminal nodes:  "  ++ show (nr_nterm_nodes res)
                ,   "Terminal nodes:     "  ++ show (nr_term_nodes res)
                ,   "Intermediate nodes: "  ++ show (nr_intermediate_nodes res)
                ,   "Packed nodes:       "  ++ show (nr_packed_nodes res)
                ,   "SPPF edges:         "  ++ show (nr_sppf_edges res)
                ,   "GSS nodes:          "  ++ show (nr_gss_nodes res)
                ,   "GSS edges:          "  ++ show (nr_gss_edges res)
                ]

