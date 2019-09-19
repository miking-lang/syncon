{-# LANGUAGE DeriveGeneric, LambdaCase, ScopedTypeVariables, FlexibleContexts, NamedFieldPuns, TupleSections, OverloadedStrings, TypeApplications #-}

module Text.Earley.Forest.SplitEpsDFA
( EpsDFA(..)
, DotProd(..)
, mkDFA
, renumberStates
, completedNT
, isCompleted
, epsDFAtoGraphViz
, defaultEpsDFAtoGraphViz
, defaultShowState
, defaultShowEdge
) where

import Prelude

import GHC.Generics (Generic)

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Tuple (swap)
import Control.Applicative ((<|>))
import Control.Arrow ((>>>), (&&&), (***))
import Control.Monad.Trans.Reader (ReaderT, runReaderT, asks)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (toList, fold)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe, isJust)
import Data.STRef (STRef, readSTRef, modifySTRef', newSTRef)
import Data.Sequence (Seq)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Data.Sequence as Seq

import Text.Earley.Forest.Grammar (TokKind, kindLabel, Parseable)
import Text.Earley.Forest.Transformations (EpsNT(..), NT(..), Rule(..), Sym(..), NullStatus(..))

data EpsDFA s t = EpsDFA
  { transitions :: !(HashMap s (HashMap (Maybe t) s))
  , initial :: !s }

data DotProd = DotProd
  !Int -- ^ Which rule, indexing into the sequence of rules
  !Int -- ^ Dotposition, indexing into syms inside the rule
  deriving (Eq, Generic)
instance Hashable DotProd

data MkDFAState s t = MkDFAState
  { inProgress :: !(STRef s (HashMap (HashSet DotProd) (HashMap (Maybe (Sym NT t)) (HashSet DotProd))))
  }

-- | Create a split LR(0) epsilon-DFA, in line with Aycock & Horspool 2002.
-- Note that this function assumes that the starting symbol(s) never occur
-- on the righthand side in any production. 'mkGrammar' will produce such
-- a grammar, and 'mkNNFGrammar' will preserve this property.
mkDFA :: forall t nodeF. (Eq (TokKind t), Hashable (TokKind t))
      => (Maybe EpsNT, Maybe EpsNT, Seq (Rule EpsNT (Sym EpsNT t) nodeF))
      -> EpsDFA (HashSet DotProd) (Sym NT t)
mkDFA (s1, s2, rules) = case s1 <|> s2 of
  Nothing -> EpsDFA M.empty S.empty
  Just (EpsNT nt _) -> runST $ do
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
              <&> ((postDot >>> fmap (either Nt Sym)) &&& (advance >>> S.singleton))
              & filter (fst >>> isJust)
              & M.fromListWith S.union
            final = M.union eps others & M.filter (S.null >>> not)
        lift $ modifySTRef' ref $ M.insert dotProds final
        mapM_ requestState final

    mPredict :: DotProd -> HashSet DotProd
    mPredict = postDot >>> maybe S.empty (either (`M.lookup` dotsByNT) mempty >>> fold)

    postDot :: DotProd -> Maybe (Either NT (TokKind t))
    postDot (DotProd ruleIdx dotIdx) = Seq.lookup ruleIdx rules  -- NOTE: this first operations should never fail, but eh
      >>= (getSyms >>> Seq.lookup dotIdx)
      >>= \case
      Nt (EpsNT nt NonNullable) -> Just $ Left nt
      Sym t -> Just $ Right t
      _ -> Nothing  -- NOTE: this should never happen, and could maybe be elided/errored upon, but for now I'm just making it a total, correct function
    getSyms (Rule _ syms _) = syms

    dotsByNT :: HashMap NT (HashSet DotProd)
    dotsByNT = [ (nt, S.singleton $ DotProd idx $ advancePastNulls 0 syms)
               | (idx, Rule nt'@(EpsNT nt status) syms _) <- [0..] `zip` toList rules
               , status == NonNullable || Just nt' == s1 || Just nt' == s2 ] -- NOTE: slightly hacky way to ensure that the starting symbol has all productions in the DFA. This is ok since the starting symbol is never in the righthand side of a production
      & M.fromListWith S.union

    advancePastNulls :: Int -> Seq (Sym EpsNT t) -> Int
    advancePastNulls startIdx syms = Seq.drop startIdx syms
      & Seq.findIndexL nonNullable
      <&> (+startIdx)
      & fromMaybe (Seq.length syms)
    nonNullable (Nt (EpsNT _ Nulling)) = False
    nonNullable _ = True

    advance :: DotProd -> DotProd
    advance (DotProd ruleIdx dotIdx) = Seq.index rules ruleIdx
      & getSyms
      & advancePastNulls (dotIdx + 1)
      & DotProd ruleIdx

    iterateInductively :: (Eq a, Hashable a) => (a -> HashSet a) -> HashSet a -> HashSet a
    iterateInductively f start = go start $ (`S.difference` start) $ foldMap f start
      where
        go prev new
          | S.null new = prev
          | otherwise = go prev' $ (`S.difference` prev') $ foldMap f new
          where prev' = prev <> new
    -- iterateInductively :: (Eq a, Hashable a) => (a -> HashSet a) -> HashSet a -> HashSet a
    -- iterateInductively f start = go start $ (`S.difference` start) $ foldMap f start
    --   where
    --     go prev new
    --       | S.null new = prev
    --       | otherwise = go (S.union prev new) new'
    --       where
    --         new' = foldMap (f >>> (`S.difference` prev)) new

completedNT :: Seq (Rule EpsNT (Sym EpsNT t) nodeF) -> DotProd -> Maybe NT
completedNT rules (DotProd ruleIdx dotIdx) = Seq.index rules ruleIdx
  & \(Rule (EpsNT nt _) syms _) -> if Seq.length syms == dotIdx then Just nt else Nothing

isCompleted :: Seq (Rule nt sym nodeF) -> DotProd -> Bool
isCompleted rules (DotProd ruleIdx dotIdx) = Seq.index rules ruleIdx
  & \(Rule _ syms _) -> Seq.length syms == dotIdx

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

defaultShowState :: forall t nodeF. Parseable t
                 => Seq (Rule EpsNT (Sym EpsNT t) nodeF)
                 -> HashSet DotProd -> Text
defaultShowState rules dots = Text.unlines $ work <$> toList dots
  where
    showSym :: Sym EpsNT t -> Text
    showSym (Nt nt) = Text.pack $ show nt
    showSym (Sym t) = kindLabel @t t
    work (DotProd ruleIdx dotIdx) = Seq.index rules ruleIdx
      & \(Rule nt syms _) ->
          let (pre, post) = Seq.splitAt dotIdx $ showSym <$> syms
          in Text.pack (show nt) <> " -> " <> Text.unwords (toList pre) <> " * " <> Text.unwords (toList post)

defaultShowEdge :: forall t. Parseable t => Sym NT t -> Text
defaultShowEdge (Nt nt) = Text.pack $ show nt
defaultShowEdge (Sym k) = kindLabel @t k

defaultEpsDFAtoGraphViz :: Parseable t
                        => Seq (Rule EpsNT (Sym EpsNT t) nodeF)
                        -> EpsDFA (HashSet DotProd) (Sym NT t) -> Text
defaultEpsDFAtoGraphViz rules = epsDFAtoGraphViz (defaultShowState rules) defaultShowEdge

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
