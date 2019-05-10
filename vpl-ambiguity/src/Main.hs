{-# OPTIONS -fno-warn-unused-imports #-}

module Main where

import Pre

import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S

import qualified Regex as R
-- import qualified ParenAutomaton as PA
import qualified GraphViz as GV

-- TODO: ensure that the "out" directory exists
-- TODO: actually run dot on the generated graphs
main :: IO ()
main = do
  -- testParenOps trivPA "trivPA"
  -- makeAndOutputParenNFA "E" trivGrammar "TrivGrammar"
  -- makeAndOutputParenNFA "E" exprGrammar "exprGrammar"
  -- makeAndOutputParenNFA "E" niceExprGrammar "niceExprGrammar"
  -- makeAndOutputParenNFA "E" arithGrammar "arithGrammar"
  -- makeAndOutputParenNFA "E" dumbArithGrammar "dumbArithGrammar"
  putStrLn @Text "Done!"

test :: IO ()
test = putStrLn @Text "test"

-- makeAndOutputParenNFA :: Text -> PA.Language Text Text -> FilePath -> IO ()
-- makeAndOutputParenNFA startNT lang path = do
--   let nfa = PA.fromLanguage startNT lang
--       dyck = PA.addDyck nfa
--       prod = PA.product (PA.mapSta Just nfa) dyck
--       coreduced = PA.reverse prod & PA.reduce & PA.reverse
--       trimmed = PA.reduce coreduced
--       trivCoreduce = PA.trivialCoReduce trimmed
--   putStrLn path
--   putStrLn @Text $ "NFA: " <> show (PA.size nfa)
--   putStrLn @Text $ "Dyck: " <> show (PA.size dyck)
--   putStrLn @Text $ "Prod: " <> show (PA.size prod)
--   putStrLn @Text $ "coreduced: " <> show (PA.size coreduced)
--   putStrLn @Text $ "trimmed: " <> show (PA.size trimmed)
--   putStrLn @Text $ "triveCoReduced: " <> show (PA.size trivCoreduce)
--   putStrLn @Text $ "unresAmbig: " <> show (PA.isUnresolvablyAmbiguous nfa)
--   writeFile ("out/" <> path <> "_1_nfa.dot") $ GV.toDotText (const "") PA.ppFakeEdge $ PA.asNFA nfa
--   writeFile ("out/" <> path <> "_2_dyck.dot") $ GV.toDotText (const "") PA.ppFakeEdge $ PA.asNFA dyck
--   writeFile ("out/" <> path <> "_3_prod.dot") $ GV.toDotText (const "") PA.ppFakeEdge $ PA.asNFA prod
--   writeFile ("out/" <> path <> "_4_coreduced.dot") $ GV.toDotText (const "") PA.ppFakeEdge $ PA.asNFA coreduced
--   writeFile ("out/" <> path <> "_5_trimmed.dot") $ GV.toDotText (const "") PA.ppFakeEdge $ PA.asNFA trimmed
--   writeFile ("out/" <> path <> "_6_trivCoreduced.dot") $ GV.toDotText (const "") PA.ppFakeEdge $ PA.asNFA trivCoreduce
--   putStrLn @Text ""

-- testParenOps :: PA.ParenNFA Int Text Text -> FilePath -> IO ()
-- testParenOps nfa path = do
--   let rev = PA.reverse nfa
--       reduced = PA.reduce nfa
--       revReduced = PA.reduce rev
--   putStrLn path
--   putStrLn @Text $ "nfa: " <> show (PA.size nfa)
--   writeFile ("out/" <> path <> "_1_nfa.dot") $ GV.toDotText show PA.ppFakeEdge $ PA.asNFA nfa
--   putStrLn @Text $ "rev: " <> show (PA.size rev)
--   writeFile ("out/" <> path <> "_2_rev.dot") $ GV.toDotText show PA.ppFakeEdge $ PA.asNFA rev
--   putStrLn @Text $ "reduced: " <> show (PA.size reduced)
--   writeFile ("out/" <> path <> "_3_reduced.dot") $ GV.toDotText show PA.ppFakeEdge $ PA.asNFA reduced
--   putStrLn @Text $ "revReduced: " <> show (PA.size revReduced)
--   writeFile ("out/" <> path <> "_4_revReduced.dot") $ GV.toDotText show PA.ppFakeEdge $ PA.asNFA revReduced

-- listRegex :: R.Regex (PA.RegexAlphabet Text Text)
-- listRegex = R.concat ["[", R.opt $ R.Concat e (R.star $ R.Concat ";" e), "]"]
--   where
--     e = R.Terminal $ PA.NT "E"

-- exprGrammar :: PA.Language Text Text
-- exprGrammar = M.fromList
--   [("E",
--     [ R.concat [e, ";", e]
--     , R.concat ["[", R.opt $ R.Concat e (R.star $ R.Concat ";" e), "]"]
--     , "a"])]
--   where
--     e = R.Terminal $ PA.NT "E"

-- trivGrammar :: PA.Language Text Text
-- trivGrammar = M.fromList
--   [("E",
--     [ R.concat [e, ";", e]
--     , "a"])]
--   where
--     e = R.Terminal $ PA.NT "E"

-- niceExprGrammar :: PA.Language Text Text
-- niceExprGrammar = M.fromList
--   [("E",
--     [ R.concat [e, ";", e]
--     , R.concat ["[", R.opt $ R.Concat e (R.star $ R.Concat "," e), "]"]
--     , "a"])]
--   where
--     e = R.Terminal $ PA.NT "E"

-- arithGrammar :: PA.Language Text Text
-- arithGrammar = M.fromList
--   [("E",
--     [ R.concat [e, "+", e]
--     , R.concat [e, "*", e]
--     , "a"])]
--   where
--     e = R.Terminal $ PA.NT "E"

-- dumbArithGrammar :: PA.Language Text Text
-- dumbArithGrammar = M.fromList
--   [("E",
--     [ R.concat [e, "+", e]
--     , R.concat [e, "*", e]
--     , R.concat [e, "*", e]
--     , "a"])]
--   where
--     e = R.Terminal $ PA.NT "E"

-- trivPA :: PA.ParenNFA Int Text Text
-- trivPA = PA.ParenNFA
--   { PA.initial = 0
--   , PA.innerTransitions = M.singleton 1 $ M.singleton "a" $ S.singleton 2
--   , PA.openTransitions = M.singleton 0 $ S.singleton ("p", 1)
--   , PA.closeTransitions = M.singleton 2 $ S.singleton ("p", 3)
--   , PA.final = 3 }
