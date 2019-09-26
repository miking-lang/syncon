{-# LANGUAGE TypeFamilies, GADTs, LambdaCase, RankNTypes, AllowAmbiguousTypes, DataKinds, FlexibleInstances #-}

module Text.Earley.Forest.Grammar
( Parseable(..)
, Prod(..)
, NodeKind(..)
, Grammar(..)

, node
, ambig

, nodeLeaf
, terminal
, label
, alts
, (<|>)
, (<--)
, (-->)
, optional
, some
, many
, noparse

, runGrammar
) where

import Prelude

import Control.Arrow ((>>>))
import Control.Monad (ap, (>=>))
import Control.Monad.Fix (MonadFix(..))
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty, (<|))
import Data.Sequence (Seq((:|>), (:<|)))
import Data.Text (Text)
import qualified Data.Sequence as Seq

class Parseable t where
  type TokKind t :: *
  getKind :: t -> TokKind t
  kindLabel :: TokKind t -> Text
  unlex :: t -> Text

-- | Smart constructor for alternatives, merges trees of alternatives into a single
-- branch, if possible. This is used by (<|>) as well, but using it directly could
-- potentially save a little bit of work.
alts :: [Prod r intLabel tok 'Interior] -> Prod r intLabel tok 'Interior
alts as = as >>= go & Alts
  where
    go (Alts as') = as'
    go a = [a]

runGrammar :: MonadFix m
           => (nodeLabel -> Prod r intLabel tok 'Interior -> m (Prod r intLabel tok 'Node))
           -> ([Prod r intLabel tok 'Node] -> m (Prod r intLabel tok 'Node))
           -> Grammar r nodeLabel intLabel tok a -> m a
runGrammar mkRule mkAmbig = \case
  RuleBind l p cont -> do
    nt <- mkRule l p
    runGrammar mkRule mkAmbig $ cont nt
  AmbigBind ps cont -> do
    nt <- mkAmbig ps
    runGrammar mkRule mkAmbig $ cont nt
  Return a -> return a
  FixBind unfixed build -> do
    a <- mfix $ runGrammar mkRule mkAmbig <$> unfixed
    runGrammar mkRule mkAmbig $ build a

data NodeKind = Node | Interior

-- | Match a single terminal with the given token kind. If the first argument is 'Just',
-- include the terminal in the parse tree, otherwise ignore it.
terminal :: Maybe intLabel -> tok -> Prod r intLabel tok 'Interior
terminal mLabel tk = Terminal (mLabel <&> pure) tk

-- | Construct a node. NOTE: will not be called if the node ends up being zero length, since
-- no zero length productions are part of the parse tree.
node :: nodeLabel -> Prod r intLabel tok 'Interior -> Grammar r nodeLabel intLabel tok (Prod r intLabel tok 'Node)
node l p = RuleBind l p return

-- | Merge these alteratives into a single ambiguous node.
ambig :: [Prod r intLabel tok 'Node] -> Grammar r nodeLabel intLabel tok (Prod r intLabel tok 'Node)
ambig ps = ps <&> go & mconcat & fold & flip AmbigBind return
  where
    go (Alts []) = Nothing
    go a = Just [a]

-- | Parse a child node.
nodeLeaf :: intLabel -> Prod r intLabel tok 'Node -> Prod r intLabel tok 'Interior
nodeLeaf l p = NonTerminalWrap (pure l) p

-- | Attach an additional label to the given production.
label :: intLabel -> Prod r intLabel tok 'Interior -> Prod r intLabel tok 'Interior
label l = \case
  Sequence ps -> Sequence $ label l <$> ps
  Terminal mLabel tk -> Terminal ((l <|) <$> mLabel) tk
  NonTerminalWrap l' p -> NonTerminalWrap (l <| l') p
  Alts as -> Alts $ label l <$> as
  Many l' p -> Many (l : l') p

-- | Make the given production optional, parsing either it or the empty sequence.
optional :: Prod r intLabel tok 'Interior -> Prod r intLabel tok 'Interior
optional p = p <|> Sequence mempty

-- | Parse the given production zero or more times.
many :: Prod r intLabel tok 'Interior -> Prod r intLabel tok 'Interior
many = Many []

-- | Parse the given production one or more times.
some :: Prod r intLabel tok 'Interior -> Prod r intLabel tok 'Interior
some p = p <> many p

data Prod r intLabel tok (k :: NodeKind) where
  Sequence :: !(Seq (Prod r intLabel tok 'Interior)) -> Prod r intLabel tok 'Interior
  Terminal :: !(Maybe (NonEmpty intLabel)) -> !tok -> Prod r intLabel tok 'Interior
  NonTerminalWrap :: !(NonEmpty intLabel) -> Prod r intLabel tok 'Node -> Prod r intLabel tok 'Interior
  NonTerminal :: !(Seq tok) -> !r -> !(Seq tok) -> Prod r intLabel tok 'Node
  Alts :: ![Prod r intLabel tok 'Interior] -> Prod r intLabel tok 'Interior
  Many :: ![intLabel] -> !(Prod r intLabel tok 'Interior) -> Prod r intLabel tok 'Interior

instance Semigroup (Prod r intLabel tok 'Interior) where
  {-# INLINE (<>) #-}
  Sequence a <> Sequence b = Sequence $ a <> b
  Sequence a <> b = Sequence $ a <> Seq.singleton b
  a <> Sequence b = Sequence $ Seq.singleton a <> b
  a <> b = Sequence $ Seq.fromList [a, b]

instance Monoid (Prod r intLabel tok 'Interior) where
  mempty = Sequence mempty
  mappend = (<>)

(-->) :: tok -> Prod r intLabel tok 'Node -> Prod r intLabel tok 'Node
t --> NonTerminal l nt r = NonTerminal (t :<| l) nt r

(<--) :: Prod r intLabel tok 'Node -> tok -> Prod r intLabel tok 'Node
NonTerminal l nt r <-- t = NonTerminal l nt (r :|> t)

(<|>) :: Prod r intLabel tok 'Interior -> Prod r intLabel tok 'Interior -> Prod r intLabel tok 'Interior
a <|> b = alts [a, b]

noparse :: Prod r intLabel tok 'Interior
noparse = alts []

data Grammar r nodeLabel intLabel tok a where
  AmbigBind :: [Prod r intLabel tok 'Node] -> (Prod r intLabel tok 'Node -> Grammar r nodeLabel intLabel tok b) -> Grammar r nodeLabel intLabel tok b
  RuleBind :: nodeLabel -> Prod r intLabel tok 'Interior -> (Prod r intLabel tok 'Node -> Grammar r nodeLabel intLabel tok b) -> Grammar r nodeLabel intLabel tok b
  FixBind :: (a -> Grammar r nodeLabel intLabel tok a) -> (a -> Grammar r nodeLabel intLabel tok b) -> Grammar r nodeLabel intLabel tok b
  Return :: a -> Grammar r nodeLabel intLabel tok a

instance Functor (Grammar r nodeLabel intLabel tok) where
  fmap f (AmbigBind p cont) = AmbigBind p (cont >>> fmap f)
  fmap f (RuleBind l p cont) = RuleBind l p (cont >>> fmap f)
  fmap f (FixBind unfixed build) = FixBind unfixed (build >>> fmap f)
  fmap f (Return x) = Return $ f x

instance Applicative (Grammar r nodeLabel intLabel tok) where
  pure = return
  (<*>) = ap

instance Monad (Grammar r nodeLabel intLabel tok) where
  return = Return
  AmbigBind p cont >>= f = AmbigBind p (cont >=> f)
  RuleBind l p cont >>= f = RuleBind l p (cont >=> f)
  FixBind unfixed build >>= f = FixBind unfixed (build >=> f)
  Return x >>= f = f x

instance MonadFix (Grammar r nodeLabel intLabel tok) where
  mfix f = FixBind f return
