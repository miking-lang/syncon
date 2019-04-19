module P4Parsing.AmbiguityReporter
( report
  , Error(..)
) where

import Pre
import Result (Result(..))

import qualified Data.HashSet as S

import Data.Functor.Foldable (project)

import P1Lexing.Types (Range, range)
import P4Parsing.Types

data Error l n
  = Ambiguity Range (HashSet (Node l n))
  | TopAmbiguity (HashSet [Node l n])
  deriving (Show)

report :: (Eq l, Hashable l, Eq n, Hashable n)
       => HashSet [Node l n] -> Result [Error l n] [Node l n]
report treeSet = case toList treeSet of
  [] -> compErr "P4Parsing.AmbiguityReporter.report" "Got an empty set of toplists"
  [tree] -> Data tree
  forest
    | equalBy length forest
    , let subforests = transpose forest
    , all (equalBy range) subforests
      -> concatMap localizeAmbiguities subforests
         <&> ((head >>> foldMap range) &&& S.fromList)
         <&> uncurry Ambiguity
         & Error
  _ -> Error [TopAmbiguity treeSet]

localizeAmbiguities :: (Eq l, Eq n) => [Node l n] -> [[Node l n]]
localizeAmbiguities forest
  | equalBy (project >>> void) forest
  , let subforests = transpose $ map (project >>> toList) forest
  , all (equalBy range) subforests
    = concatMap localizeAmbiguities subforests
  | otherwise = [forest]
