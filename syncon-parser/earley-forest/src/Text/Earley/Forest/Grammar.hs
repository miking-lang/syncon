{-# LANGUAGE TypeFamilies, GADTs, LambdaCase, RankNTypes, AllowAmbiguousTypes #-}

module Text.Earley.Forest.Grammar
( Parseable(..)
, Prod(..)
, Grammar(..)
, terminal
, ranged
, rule
, ambig
, alts
, runGrammar
) where

import Prelude

import Control.Monad (ap, (>=>))
import Control.Applicative (Alternative(..), (<**>))
import Control.Monad.Fix (MonadFix(..))
import Control.Arrow ((>>>))
import Data.Text (Text)
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.Foldable (fold)

class Parseable t where
  type TokKind t :: *
  getKind :: t -> TokKind t
  kindLabel :: TokKind t -> Text
  unlex :: t -> Text

-- | Match a single terminal with the given token kind.
terminal :: TokKind t -> Prod r t nodeF t
terminal k = Terminal k (pure id)

-- | Access the range covered by the production, in the form of the first and last
-- token covered (Nothing if the production covers the empty string).
ranged :: Prod r t nodeF (Maybe (t, t) -> a) -> Prod r t nodeF a
ranged p = Ranged p (pure id)

-- TODO: inject a name here (possibly optional), to be able to compute "while parsing <rule>"
rule :: Prod r t nodeF (nodeF r) -> Grammar r t nodeF (Prod r t nodeF r)
rule p = RuleBind p return

-- | Merge these alteratives into a single ambiguous node.
ambig :: [Prod r t nodeF r] -> Grammar r t nodeF (Prod r t nodeF r)
ambig ps = ps <&> go & mconcat & fold & flip AmbigBind return
  where
    go (Alts [] _) = Nothing
    go a = Just [a]

-- | Smart constructor for alternatives, merges trees of alternatives into a single
-- branch, if possible. This is used by (<|>) as well, but using it directly could
-- potentially save a little bit of work.
alts :: [Prod r t nodeF a] -> Prod r t nodeF (a -> b) -> Prod r t nodeF b
alts as build = case as >>= go of
  [] -> empty
  [a] -> a <**> build
  as' -> Alts as' build
  where
    go (Alts [] _) = []
    go (Alts as' (Pure f)) = fmap f <$> as'
    go a = [a]

runGrammar :: MonadFix m
           => (Prod r t nodeF (nodeF r) -> m (Prod r t nodeF r))
           -> ([Prod r t nodeF r] -> m (Prod r t nodeF r))
           -> Grammar r t nodeF a -> m a
runGrammar mkRule mkAmbig = \case
  AmbigBind ps cont -> do
    nt <- mkAmbig ps
    runGrammar mkRule mkAmbig $ cont nt
  RuleBind p cont -> do
    nt <- mkRule p
    runGrammar mkRule mkAmbig $ cont nt
  Return a -> return a
  FixBind unfixed build -> do
    a <- mfix $ runGrammar mkRule mkAmbig <$> unfixed
    runGrammar mkRule mkAmbig $ build a

data Prod r t (nodeF :: * -> *) a where
  Terminal :: !(TokKind t) -> !(Prod r t nodeF (t -> a)) -> Prod r t nodeF a
  NonTerminal :: !r -> !(Prod r t nodeF (r -> b)) -> Prod r t nodeF b
  Ranged :: !(Prod r t nodeF (Maybe (t, t) -> a)) -> !(Prod r t nodeF (a -> b))  -> Prod r t nodeF b
  Pure :: a -> Prod r t nodeF a
  Alts :: ![Prod r t nodeF a] -> !(Prod r t nodeF (a -> b)) -> Prod r t nodeF b
  Many :: !(Prod r t nodeF a) -> !(Prod r t nodeF ([a] -> b)) -> Prod r t nodeF b

data Grammar r t nodeF a where
  AmbigBind :: [Prod r t nodeF r] -> (Prod r t nodeF r -> Grammar r t nodeF b) -> Grammar r t nodeF b
  RuleBind :: Prod r t nodeF (nodeF r) -> (Prod r t nodeF r -> Grammar r t nodeF b) -> Grammar r t nodeF b
  FixBind :: (a -> Grammar r t nodeF a) -> (a -> Grammar r t nodeF b) -> Grammar r t nodeF b
  Return :: a -> Grammar r t nodeF a

instance Functor (Prod r t nodeF) where
  {-# INLINE fmap #-}
  fmap f (Terminal t build) = Terminal t $ (>>> f) <$> build
  fmap f (NonTerminal r build) = NonTerminal r $ (>>> f) <$> build
  fmap f (Ranged r build) = Ranged r $ (>>> f) <$> build
  fmap f (Pure a) = Pure $ f a
  fmap f (Alts ps build) = Alts ps $ (>>> f) <$> build
  fmap f (Many p build) = Many p $ (>>> f) <$> build

instance Applicative (Prod r t nodeF) where
  pure = Pure
  {-# INLINE (<*>) #-}
  Terminal t build <*> q = Terminal t $ flip <$> build <*> q
  NonTerminal r build <*> q = NonTerminal r $ flip <$> build <*> q
  Ranged r build <*> q = Ranged r $ flip <$> build <*> q
  Pure f <*> q = f <$> q
  Alts ps build <*> q = alts ps $ flip <$> build <*> q
  Many p build <*> q = Many p $ flip <$> build <*> q

instance Alternative (Prod r t nodeF) where
  empty = Alts [] $ pure id
  p <|> q = alts [p, q] $ pure id
  many (Alts [] _) = pure []
  many p = Many p $ Pure id
  some p = (:) <$> p <*> many p

instance Functor (Grammar r t nodeF) where
  fmap f (AmbigBind p cont) = AmbigBind p (cont >>> fmap f)
  fmap f (RuleBind p cont) = RuleBind p (cont >>> fmap f)
  fmap f (FixBind unfixed build) = FixBind unfixed (build >>> fmap f)
  fmap f (Return x) = Return $ f x

instance Applicative (Grammar r t nodeF) where
  pure = return
  (<*>) = ap

instance Monad (Grammar r t nodeF) where
  return = Return
  AmbigBind p cont >>= f = AmbigBind p (cont >=> f)
  RuleBind p cont >>= f = RuleBind p (cont >=> f)
  FixBind unfixed build >>= f = FixBind unfixed (build >=> f)
  Return x >>= f = f x

instance MonadFix (Grammar r t nodeF) where
  mfix f = FixBind f return
