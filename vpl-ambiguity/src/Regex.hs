{-# LANGUAGE TemplateHaskell #-}

module Regex
( Regex(..)
, RegexF(..)
, concat
, choice
, star
, opt
, toAutomaton
) where

import Pre hiding (concat)

import Data.String (fromString)

import qualified Data.HashMap.Lazy as M

import Data.Functor.Foldable (cata)
import Data.Functor.Foldable.TH (makeBaseFunctor)

import Automaton.DFA (DFA)
import qualified Automaton.DFA as D
import Automaton.EpsilonNFA (EpsNFA(..))
import qualified Automaton.EpsilonNFA as E

data Regex alphabet = Terminal alphabet
                    | Concat (Regex alphabet) (Regex alphabet)
                    | Choice (Regex alphabet) (Regex alphabet)
                    | Eps
                    | Kleene (Regex alphabet)

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

type AutomataM a = State Int a

newState :: AutomataM Int
newState = get <* modify (+1)

toAutomaton :: (Eq a, Hashable a) => Regex a -> DFA Int a
toAutomaton regex = evalState (cata alg regex) 1 & E.determinize & D.renumber & fst
  where
    alg (TerminalF a) = do
      s <- newState
      e <- newState
      return $ EpsNFA s (E.edge s (Just a) e) e
    alg (ConcatF a b) = do
      EpsNFA{initial = as, transitions = at, final = ae} <- a
      EpsNFA{initial = bs, transitions = bt, final = be} <- b
      let transitions = E.mergeTransitions at bt
      return $ EpsNFA as transitions be
        & E.addEdge ae Nothing bs
    alg (ChoiceF a b) = do
      EpsNFA{initial = as, transitions = at, final = ae} <- a
      EpsNFA{initial = bs, transitions = bt, final = be} <- b
      s <- newState
      e <- newState
      let transitions = E.mergeTransitions at bt
      return $ EpsNFA s transitions e
        & E.addEdge s Nothing as
        & E.addEdge s Nothing bs
        & E.addEdge ae Nothing e
        & E.addEdge be Nothing e
    alg EpsF = do
      s <- newState
      return $ EpsNFA s M.empty s
    alg (KleeneF r) = do
      a@EpsNFA{initial, final} <- r
      return $ E.addEdge final Nothing initial a
