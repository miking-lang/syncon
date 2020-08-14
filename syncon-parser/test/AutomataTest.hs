{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

module AutomataTest (test) where

import Pre hiding (State, race, diff)

import Control.Concurrent.Async.Lifted (race)

import Data.Automaton.EpsilonNVA (EpsNVA(EpsNVA), TaggedTerminal(..))
import qualified Data.Automaton.EpsilonNVA as EpsNVA
import Data.Automaton.NVA (NVA(NVA))
import qualified Data.Automaton.NVA as NVA
import Data.Automaton.DVA (DVA(DVA))
import qualified Data.Automaton.DVA as DVA

import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M

import Hedgehog hiding (test)
import qualified Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

test :: IO Bool
test = checkParallel $$(discover)

data Sigma = A | B | C deriving (Eq, Show, Enum, Bounded, Generic)
instance Hashable Sigma

data State = P | Q | R | S | T deriving (Eq, Show, Enum, Bounded, Generic)
instance Hashable State

discardSlow :: Int -> Hedgehog.TestT IO a -> Hedgehog.PropertyT IO a
discardSlow timelimit v = do
  result <- Hedgehog.test $ race (liftIO $ threadDelay timelimit) v
  case result of
    Left _ -> discard
    Right a -> pure a

failSlow :: Int -> Hedgehog.TestT IO a -> Hedgehog.PropertyT IO a
failSlow timelimit v = do
  result <- Hedgehog.test $ race (liftIO $ threadDelay timelimit) v
  case result of
    Left _ -> failure
    Right a -> pure a

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

showNVA :: (Show s, Show sta, Show i, Show o, Show c) => NVA s sta i o c -> [Char]
showNVA NVA{..} =
  "initial: " <> intercalate ", " (show <$> toList initial) <> "\n"
  <> "final: " <> intercalate ", " (show <$> toList final) <> "\n"
  <> "trans:\n" <> intercalate "\n" (sort $ inners <> opens <> closes)
  where
    inners = NVA.toTriples innerTransitions
      <&> \(p, l, q) -> "  " <> show p <> " --  " <> show l <> "     --> " <> show q
    opens = NVA.toTriples openTransitions
      <&> \(p, l, (sta, q)) -> "  " <> show p <> " -- <" <> show l <> ", +" <> show sta <> " --> " <> show q
    closes = NVA.toTriples closeTransitions
      <&> \(p, l, (sta, q)) -> "  " <> show p <> " -- >" <> show l <> ", -" <> show sta <> " --> " <> show q

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

showDVA :: (Show s, Show sta, Show i, Show o, Show c) => DVA s sta i o c -> [Char]
showDVA DVA{..} =
  "initial: " <> show initial <> "\n"
  <> "final: " <> intercalate ", " (show <$> toList final) <> "\n"
  <> "trans:\n" <> intercalate "\n" (sort $ inners <> opens <> closes)
  where
    toTriples = M.toList >=> traverse M.toList
    inners = toTriples innerTransitions
      <&> \(p, (l, q)) -> "  " <> show p <> " --  " <> show l <> "     --> " <> show q
    opens = toTriples openTransitions
      <&> \(p, (l, (sta, q))) -> "  " <> show p <> " -- <" <> show l <> ", +" <> show sta <> " --> " <> show q
    closes = (M.toList >=> traverse (M.toList >=> traverse M.toList)) closeTransitions
      <&> \(p, (l, (sta, q))) -> "  " <> show p <> " -- >" <> show l <> ", -" <> show sta <> " --> " <> show q

gDVA :: Gen (DVA State Sigma Sigma Sigma Sigma)
gDVA = do
  initial <- G.element [P ..]
  final <- G.subsequence [P ..] <&> S.fromList
  innerTransitions <- (,,) <$> G.enumBounded <*> G.enumBounded <*> G.enumBounded
    & G.list (R.linear 0 10)
    <&> fromTriples
  openTransitions <- (,,) <$> G.enumBounded <*> G.enumBounded <*> ((,) <$> G.enumBounded <*> G.enumBounded)
    & G.list (R.linear 0 10)
    <&> fromTriples
  closeTransitions <- (,,) <$> G.enumBounded <*> G.enumBounded <*> ((,) <$> G.enumBounded <*> G.enumBounded)
    & G.list (R.linear 0 10)
    <&> fromQuads
  pure DVA{..}
  where
    fromTriples = fmap (\(a, b, c) -> M.singleton a $ M.singleton b c)
      >>> foldl' (M.unionWith (<>)) mempty
    fromQuads = fmap (\(a, b, (c, d)) -> M.singleton a $ M.singleton b $ M.singleton c d)
      >>> foldl' (M.unionWith (<>)) mempty

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

prop_shortestWordIsMatched :: Property
prop_shortestWordIsMatched = withDiscards 10000 $ withTests 10000 $ property $ do
  nva <- forAllWith showNVA gNVA
  let nva' = NVA.reduce nva
  annotate $ showNVA nva'
  shouldDiscard <- discardSlow 1_000_000 $ case NVA.shortestWord nva' of
    Nothing -> return True
    Just str -> do
      annotate $ showStr str
      assert $ NVA.recognizes nva str
      assert $ NVA.recognizes nva' str
      return False
  when shouldDiscard discard

prop_differenceCorrect :: Property
prop_differenceCorrect = withTests 10000 $ property $ do
  dva1 <- forAllWith showDVA gDVA
  dva2 <- forAllWith showDVA gDVA
  str <- forAllWith showStr gStr
  let diff = DVA.difference dva1 dva2
  annotate $ showDVA diff
  let r1 = DVA.recognizes dva1 str
      r2 = DVA.recognizes dva2 str
      r3 = DVA.recognizes diff str
  (r1 && not r2) === r3

prop_determinizeCorrect :: Property
prop_determinizeCorrect = withTests 10000 $ property $ do
  nva <- forAllWith showNVA gNVA
  let dva = DVA.determinize nva
  discardSlow 1_000_000 $ annotate $ showDVA dva
  str <- forAllWith showStr gStr
  let r = NVA.recognizes nva str
      r' = DVA.recognizes dva str
  classify "any matches" $ r || r'
  r === r'
