{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module P4Parsing.Types where

import Pre

import Data.Data (Data)
import qualified Data.HashMap.Strict as M

import Data.Functor.Foldable.TH (makeBaseFunctor)

import Text.Earley.Forest.ParseTree (ParseTree(..))

import P1Lexing.Types (Range, Ranged(..))
import qualified P1Lexing.Types as P1
import qualified P2LanguageDefinition.Types as P2

data Node t = Node
  { n_name :: !P2.Name
  , n_contents :: !(HashMap P2.SDName (Seq (NodeInternals t (Node t))))
  , n_beginEnd :: !(Maybe (t, t))
  , n_range :: !Range
  } deriving (Show, Eq, Data, Typeable)

data NodeInternals t node
  = NodeLeaf !node
  | TokenLeaf !t
  | Struct !(HashMap P2.SDName (Seq (NodeInternals t node)))
  deriving (Show, Functor, Foldable, Traversable, Eq, Data, Typeable, Generic)
instance (NFData t, NFData node) => NFData (NodeInternals t node)
makeBaseFunctor ''Node
makeBaseFunctor ''NodeInternals

allNodeInternalsChildren :: NodeInternals t node -> [Either t node]
allNodeInternalsChildren (NodeLeaf n) = [Right n]
allNodeInternalsChildren (TokenLeaf t) = [Left t]
allNodeInternalsChildren (Struct m) = foldMap (foldMap allNodeInternalsChildren) m

allNodeChildren :: Node t -> [Either t (Node t)]
allNodeChildren Node{n_contents} = foldMap (foldMap allNodeInternalsChildren) n_contents

allNodeFChildren :: NodeF t n -> [Either t n]
allNodeFChildren NodeF{n_contentsF} = foldMap (foldMap allNodeInternalsChildren) n_contentsF

deriving instance (Eq t, Eq a) => Eq (NodeF t a)
deriving instance (Show t,Show a) => Show (NodeF t a)
deriving instance Generic (NodeF t a)
instance (NFData t, NFData a) => NFData (NodeF t a)

instance Ranged t => ParseTree (NodeF t) where
  type Tok (NodeF t) = t
  type InteriorF (NodeF t) = NodeInternals t
  type IntLabel (NodeF t) = P2.SDName
  type NodeLabel (NodeF t) = P2.Name

  token sdname tok = Struct $ M.singleton sdname $ pure $ TokenLeaf tok
  nodeLeaf sdname r = Struct $ M.singleton sdname $ pure $ NodeLeaf r
  label sdname int = Struct $ M.singleton sdname $ pure int
  node name mLocation int = NodeF name contents mLocation r
    where
      contents = case int of
        Struct int' -> int'
        _ -> M.empty
      r = maybe P1.Nowhere (uncurry (mappend `on` range)) mLocation

  Struct a <-> Struct b = Struct $ M.unionWith (<>) a b
  a@Struct{} <-> _ = a
  _ <-> a@Struct{} = a
  _ <-> _ = Struct M.empty
  emptyInt = Struct M.empty

-- | Datatype to give the lexer to convince it to only parse a single language.
data SingleLanguage = SingleLanguage deriving (Show, Eq, Ord, Generic, Data, Typeable)
instance Hashable SingleLanguage
instance NFData SingleLanguage
type SL = SingleLanguage

instance Ranged (Node t) where
  range = n_range
instance Ranged (NodeF t x) where
  range = n_rangeF

instance (Hashable t) => Hashable (Node t) where
  hashWithSalt = hashUsing $ \(Node n c be r) ->
    (n, toList <$> c, be, r)
instance (Hashable t, Hashable node) => Hashable (NodeF t node) where
  hashWithSalt = hashUsing $ \(NodeF n c be r) ->
    (n, toList <$> c, be, r)

instance (Hashable t, Hashable node) => Hashable (NodeInternals t node) where
  hashWithSalt s (NodeLeaf n) = s `hashWithSalt` (0::Int) `hashWithSalt` n
  hashWithSalt s (TokenLeaf t) = s `hashWithSalt` (1::Int) `hashWithSalt` t
  hashWithSalt s (Struct m) = s `hashWithSalt` (2::Int) `hashWithSalt` (toList <$> m)
