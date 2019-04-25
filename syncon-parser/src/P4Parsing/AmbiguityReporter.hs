module P4Parsing.AmbiguityReporter
( report
  , Error(..)
) where

import Pre
import Result (Result(..))

import Text.Printf (printf)
import qualified Data.Text as Text
import qualified Data.HashSet as S

import Data.Functor.Foldable (project)

import ErrorMessage (FormatError(..), simpleErrorMessage)

import P1Lexing.Types (Range, range, textualRange)
import P2LanguageDefinition.Types (Name(..))
import P4Parsing.Types

data Error l n
  = Ambiguity Range (HashSet (Node l n))
  | TopAmbiguity (HashSet [Node l n])
  deriving (Show)

instance FormatError (Error l n) where
  formatError (Ambiguity r trees) = simpleErrorMessage r $
    "Ambiguity error with " <> show (length trees) <> " alternatives.\n" <>
    foldMap ((n_name &&& (project >>> toList)) >>> format) trees
    where
      format (name, children) = coerce name <> formatChildren children <> "\n"
      formatChildren = sortBy (compare `on` range) >>> foldMap formatNode
      formatNode n@Node{n_name = Name n'} =
        printf "\n - %- 20s %s" n' (textualRange $ range n) & Text.pack
  formatError (TopAmbiguity trees) = simpleErrorMessage mempty $
    "The top level is ambiguous. Here are the alternatives:\n"
    <> foldMap (foldMap formatNode >>> (<> "\n\n")) trees
    where
      formatNode n@Node{n_name = Name n'} =
        printf "- %- 20s %s" n' (textualRange $ range n) & Text.pack

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
