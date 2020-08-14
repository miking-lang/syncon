{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

module AutomataTest (test) where

import Pre hiding (State)

import Data.Automaton.EpsilonNVA (EpsNVA(EpsNVA), TaggedTerminal(..))
import qualified Data.Automaton.EpsilonNVA as EpsNVA
import Data.Automaton.NVA (NVA(NVA))
import qualified Data.Automaton.NVA as NVA

import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M

import Hedgehog hiding (test)
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

test :: IO Bool
test = checkParallel $$(discover)

data Sigma = A | B | C deriving (Eq, Show, Enum, Bounded, Generic)
instance Hashable Sigma

data State = P | Q | R | S | T deriving (Eq, Show, Enum, Bounded, Generic)
instance Hashable State

showStr :: (Show i, Show o, Show c) => [TaggedTerminal i o c] -> [Char]
showStr = fmap fmt >>> intercalate " " >>> \case
  [] -> "<empty>"
  x -> x
  where
    fmt (Open o) = "<" <> show o
    fmt (Close c) = show c <> ">"
    fmt (Inner i) = show i

gTaggedTerminal :: Gen (TaggedTerminal Sigma Sigma Sigma)
gTaggedTerminal = G.choice
  [ Inner <$> G.enumBounded
  , Open <$> G.enumBounded
  , Close <$> G.enumBounded
  ]

gStr :: Gen [TaggedTerminal Sigma Sigma Sigma]
gStr = G.list (R.linear 0 10) gTaggedTerminal

showEpsNVA :: (Show s, Show sta, Show i, Show o, Show c) => EpsNVA s sta i o c -> [Char]
showEpsNVA EpsNVA{..} =
  "initial: " <> intercalate ", " (show <$> toList initial) <> "\n"
  <> "final: " <> intercalate ", " (show <$> toList final) <> "\n"
  <> "trans:\n" <> intercalate "\n" (sort $ inners <> opens <> closes)
  where
    inners = EpsNVA.toTriples innerTransitions
      <&> \(p, l, q) -> "  " <> show p <> " --  " <> maybe "_" show l <> "     --> " <> show q
    opens = EpsNVA.toTriples openTransitions
      <&> \(p, l, (sta, q)) -> "  " <> show p <> " -- <" <> show l <> ", +" <> show sta <> " --> " <> show q
    closes = EpsNVA.toTriples closeTransitions
      <&> \(p, l, (sta, q)) -> "  " <> show p <> " -- >" <> show l <> ", -" <> show sta <> " --> " <> show q

showNVA :: (Show s, Show sta, Show i, Show o, Show c) => NVA s sta i o c -> [Char]
showNVA NVA{..} =
  "initial: " <> intercalate ", " (show <$> toList initial) <> "\n"
  <> "final: " <> intercalate ", " (show <$> toList final) <> "\n"
  <> "trans:\n" <> intercalate "\n" (sort $ inners <> opens <> closes)
  where
    inners = EpsNVA.toTriples innerTransitions
      <&> \(p, l, q) -> "  " <> show p <> " --  " <> show l <> "     --> " <> show q
    opens = EpsNVA.toTriples openTransitions
      <&> \(p, l, (sta, q)) -> "  " <> show p <> " -- <" <> show l <> ", +" <> show sta <> " --> " <> show q
    closes = EpsNVA.toTriples closeTransitions
      <&> \(p, l, (sta, q)) -> "  " <> show p <> " -- >" <> show l <> ", -" <> show sta <> " --> " <> show q

gEps :: Gen (EpsNVA State Sigma Sigma Sigma Sigma)
gEps = do
  initial <- G.subsequence [P ..] & G.filter (null >>> not) <&> S.fromList
  final <- G.subsequence [P ..] <&> S.fromList
  innerTransitions <- (,,) <$> G.enumBounded <*> G.maybe G.enumBounded <*> G.enumBounded
    & G.list (R.linear 0 10)
    <&> EpsNVA.fromTriples
  openTransitions <- (,,) <$> G.enumBounded <*> G.enumBounded <*> ((,) <$> G.enumBounded <*> G.enumBounded)
    & G.list (R.linear 0 10)
    <&> EpsNVA.fromTriples
  closeTransitions <- (,,) <$> G.enumBounded <*> G.enumBounded <*> ((,) <$> G.enumBounded <*> G.enumBounded)
    & G.list (R.linear 0 10)
    <&> EpsNVA.fromTriples
  pure EpsNVA{..}

gNVA :: Gen (NVA State Sigma Sigma Sigma Sigma)
gNVA = do
  initial <- G.subsequence [P ..] & G.filter (null >>> not) <&> S.fromList
  final <- G.subsequence [P ..] <&> S.fromList
  innerTransitions <- (,,) <$> G.enumBounded <*> G.enumBounded <*> G.enumBounded
    & G.list (R.linear 0 10)
    <&> NVA.fromTriples
  openTransitions <- (,,) <$> G.enumBounded <*> G.enumBounded <*> ((,) <$> G.enumBounded <*> G.enumBounded)
    & G.list (R.linear 0 10)
    <&> NVA.fromTriples
  closeTransitions <- (,,) <$> G.enumBounded <*> G.enumBounded <*> ((,) <$> G.enumBounded <*> G.enumBounded)
    & G.list (R.linear 0 10)
    <&> NVA.fromTriples
  pure NVA{..}

-- TODO: better printing of automata and strings (and fix the intentional bug in EpsNVA.recognizes

prop_EpsToNVACorrectBroad :: Property
prop_EpsToNVACorrectBroad = withTests 10000 $ property $ do
  enva <- forAllWith showEpsNVA gEps
  let nva = NVA.fromEpsNVA enva
  annotate $ showNVA nva
  str <- forAllWith showStr gStr
  let erec = EpsNVA.recognizes enva str
      nrec = NVA.recognizes nva str
  classify "any matches" $ erec || nrec
  erec === nrec

prop_trivCoReduceCorrect :: Property
prop_trivCoReduceCorrect = withTests 10000 $ property $ do
  nva <- forAllWith showNVA gNVA
  let nva' = NVA.trivialCoReduce nva
  annotate $ showNVA nva'
  str <- forAllWith showStr gStr
  let r = NVA.recognizes nva str
      r' = NVA.recognizes nva' str
  classify "any matches" $ r || r'
  r === r'

prop_ReduceCorrect :: Property
prop_ReduceCorrect = withTests 10000 $ property $ do
  nva <- forAllWith showNVA gNVA
  let nva' = NVA.reduce nva
  annotate $ showNVA nva'
  str <- forAllWith showStr gStr
  let r = NVA.recognizes nva str
      r' = NVA.recognizes nva' str
  classify "any matches" $ r || r'
  r === r'
