module P5DynamicAmbiguity.PatchTokenStream (patch, ambiguityTokLength) where

import Pre

import Data.Sequence (pattern (:<|), pattern (:|>), pattern Empty)
import qualified Data.Sequence as Seq

import qualified P1Lexing.Types as P1
import qualified P2LanguageDefinition.Types as P2
import qualified P4Parsing.Types as P4
import qualified P5DynamicAmbiguity.Types as P5

type Token5 elidable = P5.Token elidable
type Token1 l = P1.Token l P2.TypeName

patch :: Eq l => l -> (elidable -> (Token1 l, Token1 l))
      -> (Token1 l, Token1 l) -> Seq (Token5 elidable) -> Seq (Token1 l) -> Seq (Token1 l)
patch l getBounds bounds toSpliceIn original = prefix <> splice l getBounds toSpliceIn middle <> postfix
  where
    (prefix, middle, postfix) = triSplit bounds original


splice :: Eq l => l -> (elidable -> (Token1 l, Token1 l)) -> Seq (Token5 elidable) -> Seq (Token1 l) -> Seq (Token1 l)
splice l getBounds = recur
  where
    unexpectedElided = compErr "P5DynamicAmbiguity.PatchTokenStream.splice" "unexpected elided token"

    recur spliceSeq original = Seq.spanl (not . isElided) spliceSeq
      & first (P5.makeFakeToken l unexpectedElided <$>)
      & \case
      (nonElided, Empty) -> nonElided
      (nonElided, P5.ElidedTok e :<| spliceSeq') ->
        let (_, elided, original') = triSplit (getBounds e) original
        in nonElided <> elided <> recur spliceSeq' original'
      (_, _ :<| _) -> compErr "P5DynamicAmbiguity.PatchTokenStream.splice.recur" "expected an elided token"

triSplit :: Eq a => (a, a) -> Seq a -> (Seq a, Seq a, Seq a)
triSplit (firstA, lastA) = Seq.spanl (/= firstA)
  >>> fmap (spanInclEx (/= lastA))
  >>> \(prefix, (middle, postfix)) -> (prefix, middle, postfix)
  where
    spanInclEx :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
    spanInclEx f s = case Seq.spanl f s of
      (prefix, match :<| postfix) -> (prefix :|> match, postfix)
      _ -> compErr "P5DynamicAmbiguity.PatchTokenStream.triSplit.spanInclEx" "no matching element"

-- | Count the number of tokens involved in an ambiguity. This counts each elided node as a single
-- token, instead of counting all the tokens contained in it, since those shouldn't affect the
-- running time of the dynamic analysis.
ambiguityTokLength :: forall elidable l. Eq l => (elidable -> (Token1 l, Token1 l)) -> Seq (Token1 l) -> P5.NodeOrElide elidable (Token1 l) -> Int
ambiguityTokLength getBounds' stream ambRepresentative =
  getElided ambRepresentative
  <&> getBounds'
  <&> ((`triSplit` ambCover) >>> \(_, mid, _) -> length mid - 1)
  & sum
  & (length ambCover - )
  where
    (_, ambCover, _) = triSplit bounds stream
    bounds = case ambRepresentative of
      P5.Node n -> P4.n_beginEndF n & compFromJust "P5DynamicAmbiguity.PatchTokenStream.ambiguityTokLength" "missing beginEnd"
      P5.Elide e -> getBounds' e
    getElided = \case
      P5.Node n -> foldMap getElided n
      P5.Elide e -> Seq.singleton e

isElided :: Token5 elidable -> Bool
isElided P5.ElidedTok{} = True
isElided _ = False
