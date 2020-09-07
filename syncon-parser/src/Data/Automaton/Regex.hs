{-# LANGUAGE TemplateHaskell #-}

module Data.Automaton.Regex
( Regex(..)
, RegexF(..)
, concat
, choice
, star
, opt
, shortestWord
, toAutomaton
) where

import Pre hiding (concat)

import Data.String (fromString)

import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import qualified Data.Sequence as Seq

import Data.Functor.Foldable (cata)
import Data.Functor.Foldable.TH (makeBaseFunctor)

import qualified Data.Automaton as FA
import qualified Data.Automaton.EpsilonNFA as E

data Regex alphabet = Terminal alphabet
                    | Concat (Regex alphabet) (Regex alphabet)
                    | Choice (Regex alphabet) (Regex alphabet)
                    | Eps
                    | Kleene (Regex alphabet)
                    deriving (Show)
instance Semigroup (Regex a) where
  Eps <> b = b
  a <> Eps = a
  a <> b = Concat a b
instance Monoid (Regex a) where
  mappend = (<>)
  mempty = Eps

makeBaseFunctor ''Regex

instance IsString a => IsString (Regex a) where
  fromString = fromString >>> Terminal

concat :: [Regex a] -> Regex a
concat = foldl' Concat Eps

choice :: [Regex a] -> Regex a
choice (r : rs) = foldl' Choice r rs
choice [] = compErr "Regex.choice" "Got no choices"

star :: Regex a -> Regex a
star = Kleene

opt :: Regex a -> Regex a
opt r = Choice Eps r

-- | Produces a shortest word recognized by a given 'Regex'.
shortestWord :: Regex a -> Seq a
shortestWord = cata $ \case
  TerminalF a -> Seq.singleton a
  ConcatF a b -> a <> b
  ChoiceF a b
    | Seq.length a <= Seq.length b -> a
    | otherwise -> b
  EpsF -> Seq.empty
  KleeneF _ -> Seq.empty

type AutomataM a = State Int a
data Automaton s a = Automaton
  { initial :: s
  , transitions :: HashMap s (HashMap (Maybe a) (HashSet s))
  , final :: s }

addEdge :: (Eq s, Hashable s, Eq a, Hashable a) => s -> Maybe a -> s -> Automaton s a -> Automaton s a
addEdge start t end a@Automaton{transitions} = a
  { transitions = transitions & M.insertWith (M.unionWith S.union) start (M.singleton t (S.singleton end)) }

newState :: AutomataM Int
newState = get <* modify (+1)

toAutomaton :: (Eq a, Hashable a) => Regex a -> FA.EpsNFA Int a
toAutomaton regex = evalState (cata alg regex) 1 & toEpsNFA
  where
    toEpsNFA Automaton{initial, transitions, final} = FA.EpsNFA initial transitions $ S.singleton final
    alg (TerminalF a) = do
      s <- newState
      e <- newState
      return $ Automaton s (E.edge s (Just a) e) e
    alg (ConcatF a b) = do
      Automaton{initial = as, transitions = at, final = ae} <- a
      Automaton{initial = bs, transitions = bt, final = be} <- b
      let transitions = E.mergeTransitions at bt
      return $ Automaton as transitions be
        & addEdge ae Nothing bs
    alg (ChoiceF a b) = do
      Automaton{initial = as, transitions = at, final = ae} <- a
      Automaton{initial = bs, transitions = bt, final = be} <- b
      s <- newState
      e <- newState
      let transitions = E.mergeTransitions at bt
      return $ Automaton s transitions e
        & addEdge s Nothing as
        & addEdge s Nothing bs
        & addEdge ae Nothing e
        & addEdge be Nothing e
    alg EpsF = do
      s <- newState
      return $ Automaton s M.empty s
    alg (KleeneF r) = do
      a@Automaton{initial, final} <- r
      return $ addEdge final Nothing initial a
        & addEdge initial Nothing final
