module Main where

import Pre

import qualified Data.HashMap.Lazy as M

import qualified Regex as R
import qualified ParenAutomaton as PA
import qualified GraphViz as GV

-- TODO: ensure that the "out" directory exists
-- TODO: actually run dot on the generated graphs
main :: IO ()
main = do
  makeAndOutputParenNFA "E" exprGrammar "ExprGrammar"

makeAndOutputParenNFA :: Text -> PA.Language Text Text -> FilePath -> IO ()
makeAndOutputParenNFA startNT lang path = do
  let nfa = PA.fromLanguage startNT lang
      dyck = PA.addDyck nfa
      prod = PA.product (PA.mapSta Just nfa) dyck
  putStrLn @Text $ "NFA: " <> show (PA.size nfa)
  putStrLn @Text $ "Dyck: " <> show (PA.size dyck)
  putStrLn @Text $ "Prod: " <> show (PA.size prod)
  writeFile ("out/" <> path <> "1.dot") $ GV.toDotText (const "") PA.ppFakeEdge $ PA.asNFA nfa
  writeFile ("out/" <> path <> "2.dot") $ GV.toDotText (const "") PA.ppFakeEdge $ PA.asNFA dyck
  writeFile ("out/" <> path <> "3.dot") $ GV.toDotText (const "") PA.ppFakeEdge $ PA.asNFA prod

listRegex :: R.Regex (PA.RegexAlphabet Text Text)
listRegex = R.concat ["[", R.opt $ R.Concat e (R.star $ R.Concat ";" e), "]"]
  where
    e = R.Terminal $ PA.NT "E"

exprGrammar :: PA.Language Text Text
exprGrammar = M.fromList
  [("E",
    [ R.concat [e, ";", e]
    , R.concat ["[", R.opt $ R.Concat e (R.star $ R.Concat ";" e), "]"]
    , "a"])]
  where
    e = R.Terminal $ PA.NT "E"
