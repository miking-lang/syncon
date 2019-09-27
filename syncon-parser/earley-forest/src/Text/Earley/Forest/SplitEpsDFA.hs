{-# LANGUAGE DeriveGeneric, LambdaCase, ScopedTypeVariables, FlexibleContexts, NamedFieldPuns, TupleSections, OverloadedStrings, TypeApplications, AllowAmbiguousTypes #-}

module Text.Earley.Forest.SplitEpsDFA
( EpsDFA(..)
, DotProd(..)
, mkDFA
, renumberStates
, completedNT
, isCompleted
, dotToRule
, epsDFAtoGraphViz
, defaultEpsDFAtoGraphViz
, defaultShowState
, defaultShowEdge
) where

import Prelude

import GHC.Generics (Generic)

import Codec.Serialise (Serialise)
import Control.Arrow ((>>>), (&&&), (***))
import Control.DeepSeq (NFData)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, asks)
import Data.Foldable (toList, fold)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.Maybe (isJust)
import Data.STRef (STRef, readSTRef, modifySTRef', newSTRef)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Tuple (swap)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Data.Sequence as Seq
import qualified Data.Text as Text

import Text.Earley.Forest.Grammar (TokKind, kindLabel, Parseable)
import Text.Earley.Forest.Transformations (NT(..), Rule(..), ruleAsTuple, Sym(..))

data EpsDFA s t = EpsDFA
  { transitions :: !(HashMap s (HashMap (Maybe t) s))
  , initial :: !s
  } deriving (Generic)
instance (NFData s, NFData t) => NFData (EpsDFA s t)
instance (Eq s, Hashable s, Eq t, Hashable t, Serialise s, Serialise t) => Serialise (EpsDFA s t)

data DotProd = DotProd
  !Int -- ^ Which rule, indexing into the sequence of rules
  !Int -- ^ Dotposition, indexing into syms inside the rule
  deriving (Eq, Generic)
instance Hashable DotProd

data MkDFAState s t = MkDFAState
  { inProgress :: !(STRef s (HashMap (HashSet DotProd) (HashMap (Maybe (Either NT t)) (HashSet DotProd))))
  }

-- | Create a split LR(0) epsilon-DFA, in line with Aycock & Horspool 2002.
-- Note that this function assumes that the starting symbol(s) never occur
-- on the righthand side in any production. 'mkGrammar' will produce such
-- a grammar, and 'mkNNFGrammar' will preserve this property.
mkDFA :: forall t intLabel. (Eq t, Hashable t)
      => (Maybe NT, Bool, Seq (Rule NT t (Sym intLabel NT t)))
      -> EpsDFA (HashSet DotProd) (Either NT t)
mkDFA (productiveStart, _, rules) = case productiveStart of
  Nothing -> EpsDFA M.empty S.empty
  Just nt -> runST $ do
    state@MkDFAState{inProgress} <- MkDFAState <$> newSTRef M.empty
    let initial = M.lookup nt dotsByNT & fold
    runReaderT (requestState initial) state
    transitions <- readSTRef inProgress
    return EpsDFA{transitions, initial}
  where
    requestState :: HashSet DotProd -> ReaderT (MkDFAState s t) (ST s) ()
    requestState dotProds = do
      ref <- asks inProgress
      done <- lift $ readSTRef ref <&> M.member dotProds
      if done then return () else do
        let eps = M.singleton Nothing $ iterateInductively mPredict dotProds `S.difference` dotProds
            others = toList dotProds
              <&> (postDot &&& (advance >>> S.singleton))
              & filter (fst >>> isJust)
              & M.fromListWith S.union
            final = M.union eps others & M.filter (S.null >>> not)
        lift $ modifySTRef' ref $ M.insert dotProds final
        mapM_ requestState final

    mPredict :: DotProd -> HashSet DotProd
    mPredict = postDot >>> maybe S.empty (either (`M.lookup` dotsByNT) mempty >>> fold)

    postDot :: DotProd -> Maybe (Either NT t)
    postDot (DotProd ruleIdx dotIdx) = Seq.lookup ruleIdx rules  -- NOTE: this first operations should never fail, but eh
      >>= (getSyms >>> Seq.lookup dotIdx)
      >>= \case
      Nt nt _ -> Just $ Left nt
      Sym t _ _ -> Just $ Right t
    getSyms = ruleAsTuple >>> snd

    dotsByNT :: HashMap NT (HashSet DotProd)
    dotsByNT = [ (nt, S.singleton $ DotProd idx 0)
               | (idx, (nt, _)) <- [0..] `zip` fmap ruleAsTuple (toList rules) ]
      & M.fromListWith S.union

    advance :: DotProd -> DotProd
    advance (DotProd ruleIdx dotIdx) = DotProd ruleIdx (dotIdx + 1)

    iterateInductively :: (Eq a, Hashable a) => (a -> HashSet a) -> HashSet a -> HashSet a
    iterateInductively f start = go start $ (`S.difference` start) $ foldMap f start
      where
        go prev new
          | S.null new = prev
          | otherwise = go prev' $ (`S.difference` prev') $ foldMap f new
          where prev' = prev <> new

dotToRule :: Seq (Rule nt t sym) -> DotProd -> (Int, Rule nt t sym)
dotToRule rules (DotProd ruleIdx _) = (ruleIdx, Seq.index rules ruleIdx)

completedNT :: Seq (Rule NT t (Sym intLabel NT t)) -> DotProd -> Maybe NT
completedNT rules (DotProd ruleIdx dotIdx) = Seq.index rules ruleIdx
  & ruleAsTuple
  & \(nt, syms) -> if Seq.length syms == dotIdx then Just nt else Nothing

isCompleted :: Seq (Rule nt t (Sym intLabel nt t)) -> DotProd -> Bool
isCompleted rules (DotProd ruleIdx dotIdx) = Seq.index rules ruleIdx
  & ruleAsTuple
  & \(_, syms) -> Seq.length syms == dotIdx

-- | Replace all states by integers, and also return a map with sufficient information to reverse
-- the operation.
renumberStates :: (Eq s, Hashable s) => EpsDFA s t -> (HashMap Int s, EpsDFA Int t)
renumberStates dfa@EpsDFA{initial, transitions} = EpsDFA
  { transitions = M.toList transitions
    <&> (translate *** fmap translate)
    & M.fromList
  , initial = translate initial }
  & (revStateMap,)
  where
    translate s = case M.lookup s stateMap of
      Nothing -> error "Missing state in renumberStates"
      Just s' -> s'
    stateMap = runST $ do
      ref <- newSTRef 0
      states dfa
        & S.toMap
        & traverse (\_ -> readSTRef ref <* modifySTRef' ref (+1))
    revStateMap = M.toList stateMap <&> swap & M.fromList

states :: (Eq s, Hashable s) => EpsDFA s t -> HashSet s
states EpsDFA{initial, transitions} = M.toList transitions
  & foldMap (\(s, m) -> foldMap S.singleton m & S.insert s)
  & S.insert initial

defaultShowState :: forall t intLabel. Parseable t
                 => Seq (Rule NT (TokKind t) (Sym intLabel NT (TokKind t)))
                 -> HashSet DotProd -> Text
defaultShowState rules dots = Text.unlines $ work <$> toList dots
  where
    showSym :: Sym intLabel NT (TokKind t) -> Text
    showSym (Nt nt _) = Text.pack $ show nt
    showSym (Sym t _ _) = kindLabel @t t
    work (DotProd ruleIdx dotIdx) = Seq.index rules ruleIdx
      & ruleAsTuple
      & \(nt, syms) ->
          let (pre, post) = Seq.splitAt dotIdx $ showSym <$> syms
          in Text.pack (show nt) <> " -> " <> Text.unwords (toList pre) <> " * " <> Text.unwords (toList post)

defaultShowEdge :: forall t. Parseable t => Either NT (TokKind t) -> Text
defaultShowEdge (Left nt) = Text.pack $ show nt
defaultShowEdge (Right k) = kindLabel @t k

defaultEpsDFAtoGraphViz :: forall t intLabel. Parseable t
                        => Seq (Rule NT (TokKind t) (Sym intLabel NT (TokKind t)))
                        -> EpsDFA (HashSet DotProd) (Either NT (TokKind t)) -> Text
defaultEpsDFAtoGraphViz rules = epsDFAtoGraphViz @_ @(Either NT (TokKind t)) (defaultShowState @t rules) (defaultShowEdge @t)

epsDFAtoGraphViz :: (Eq s, Hashable s) => (s -> Text) -> (t -> Text) -> EpsDFA s t -> Text
epsDFAtoGraphViz showState showEdge dfa = "digraph {\n"
  -- <> "  rankdir=LR;\n"
  <> "  startState [shape=point, label=\"\"];\n"
  <> "  startState -> " <> Text.pack (show initial) <> ";\n"
  <> foldMap genState (M.toList translations)
  <> foldMap genEdges (M.toList transitions)
  <> "}"
  where
    (translations, EpsDFA{initial, transitions}) = renumberStates dfa
    genState (idx, oldState) = "  " <> Text.pack (show idx) <> " [label=\"" <> Text.pack (show idx) <> "\\n" <> sToStr oldState <> "\",shape=rect];\n"
    sToStr = showState >>> escape
    escape = Text.concatMap $ \case
      '\\' -> "\\\\"
      '"' -> "\\\""
      '\n' -> "\\n"
      c -> Text.singleton c
    genEdges (idx, m) = M.toList m & foldMap (genEdge idx)
    genEdge s1 (t, s2) = "  " <> Text.pack (show s1) <> " -> " <> Text.pack (show s2) <> "[label=\"" <> tToStr t <> "\"];\n"
    tToStr (Just t) = showEdge t & escape
    tToStr Nothing = ""
