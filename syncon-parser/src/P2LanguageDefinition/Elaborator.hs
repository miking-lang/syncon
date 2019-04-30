module P2LanguageDefinition.Elaborator
( elaborate
) where

import Pre

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S

import Data.Generics.Uniplate.Data (universe)

import P2LanguageDefinition.Types

-- | If an infix operator has an associativity, it is one of these two. We don't care in any other
-- case, because different precedence levels don't care about associativity, and differing
-- associativities on the same levels merely have to stay undefined, there is no good definition.
data Assoc = AssocLeft | AssocRight

-- | Produce a single structure containing all disambiguation. Note that there won't be a mapping
-- for the (qualified) sdnames that have no marks, i.e., conceptually the total map is obtained
-- through `M.lookupDefault S.empty (name, sdname) (elaborate ...)`.
elaborate :: HashMap Name Syncon  -- ^ All syncons we will work with
          -> Seq Forbid  -- ^ Forbids, including generated associativity forbids
          -> PrecedenceMatrix  -- ^ All defined precedences
          -> Elaboration  -- ^ A mapping from (qualified) sdnames to their marks, i.e., their forbidden syncons
elaborate syncons forbids (PrecedenceMatrix mat) = elaboratedForbids : assocEntries
  & foldl' (M.unionWith S.union) M.empty
  where
    elaboratedForbids = toList forbids
      & fmap (\case
                 Forbid _ (_, n1) (_, sdname) (_, n2) -> ((n1, Right sdname), S.singleton n2)
                 ForbidRec _ (_, n1) (_, r) (_, n2) -> ((n1, Left r), S.singleton n2))
      & M.fromListWith S.union

    recs = syncons <&> getSDRecs

    -- Figure out associativity for all operator syncons. There is no entry if there is no associativity.
    assocs :: HashMap Name Assoc
    assocs = recs & M.mapMaybeWithKey getAssoc
    getAssoc name recs' = case (LRec `S.member` recs', RRec `S.member` recs') of
      (False, False) -> Nothing  -- not an operator
      (True, False) -> Just AssocLeft  -- unary postfix
      (False, True) -> Just AssocRight  -- unary prefix
      (True, True) -> case (hasAssocForbid name LRec, hasAssocForbid name RRec) of
        (False, False) -> Nothing  -- no associativity
        (True, False) -> Just AssocRight  -- binary right associative
        (False, True) -> Just AssocLeft  -- binary right associative
        (True, True) -> compErr "P2LanguageDefinition.Elaborator.elaborate.getAssoc" $ show name <> " has forbids for both SDLeft and SDRight."
    hasAssocForbid name r = M.lookup (name, Left r) elaboratedForbids & fold & S.member name

    -- NOTE: (<>) is ok here because we know that their keySets are disjoint
    processEntry :: (Name, Name) -> Ordering -> Elaboration
    processEntry (n1, n2) EQ = case (M.lookup n1 assocs, M.lookup n2 assocs) of
      (Just AssocLeft, Just AssocLeft) -> forbid n1 RRec n2 <> forbid n2 RRec n1
      (Just AssocRight, Just AssocRight) -> forbid n1 LRec n2 <> forbid n2 LRec n1
      _ -> M.empty
    processEntry (n1, n2) LT = forbid n2 LRec n1 <> forbid n2 RRec n1 <> forbid n2 Rec n1
    processEntry (n1, n2) GT = forbid n1 LRec n2 <> forbid n1 RRec n2 <> forbid n1 Rec n2

    assocEntries = mat <&> fst & M.mapWithKey processEntry & toList

    forbid n1 r n2 = if M.lookup n1 recs & fold & S.member r
      then M.singleton (n1, Left r) (S.singleton n2)
      else M.empty

-- | Find all 'Rec's present in the syntax description of a syncon
getSDRecs :: Syncon -> HashSet Rec
getSDRecs = s_syntaxDescription >>> inner >>> S.fromList
  where
    inner descr = [r | SDRec _ r <- universe descr]
