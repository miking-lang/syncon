module P4Parsing.ForestParser.Grammar
( Prod(..)
, Grammar(..)
, rule
, terminal
, ambig
, alts
, runGrammar
) where

import Pre

import Control.Monad.Fix (MonadFix(..))

-- This will form a sort of "linked list" of symbols in the right hand side of a single production
data Prod r t nodeF a where
  Terminal :: !(t -> Bool) -> !(Prod r t nodeF (t -> b)) -> Prod r t nodeF b
  NonTerminal :: !r -> !(Prod r t nodeF (r -> a)) -> Prod r t nodeF a
  Pure :: a -> Prod r t nodeF a
  Ambig :: ![Prod r t nodeF r] -> !(Prod r t nodeF (r -> b)) -> Prod r t nodeF b
  Alts :: ![Prod r t nodeF a] -> !(Prod r t nodeF (a -> b)) -> Prod r t nodeF b
  Many :: !(Prod r t nodeF a) -> !(Prod r t nodeF ([a] -> b)) -> Prod r t nodeF b

-- | Match a token for which the given predicate returns @Just a@,
-- and return the @a@.
terminal :: (t -> Bool) -> Prod r t nodeF t
terminal p = Terminal p $ Pure identity

instance Functor (Prod r t nodeF) where
  {-# INLINE fmap #-}
  fmap f (Terminal t build) = Terminal t $ (>>> f) <$> build
  fmap f (NonTerminal r build) = NonTerminal r $ (>>> f) <$> build
  fmap f (Pure a) = Pure $ f a
  fmap f (Ambig ps build) = Ambig ps $ (>>> f) <$> build
  fmap f (Alts ps build) = Alts ps $ (>>> f) <$> build
  fmap f (Many p build) = Many p $ (>>> f) <$> build

alts :: [Prod r t nodeF a] -> Prod r t nodeF (a -> b) -> Prod r t nodeF b
alts as build = case as >>= go of
  [] -> empty
  [a] -> a <**> build
  as' -> Alts as' build
  where
    go (Alts [] _) = []
    go (Alts as' (Pure f)) = fmap f <$> as'
    go a = [a]

-- | Merge these alteratives into a single ambiguous node. Note that this operation is associative
-- and commutative, but not idempotent. Note also that it will not merge through '<|>'.
ambig :: [Prod r t nodeF r] -> Prod r t nodeF r
ambig = (>>= go) >>> \case
  [] -> empty
  [p] -> p
  ps -> Ambig ps $ pure identity
  where
    go (Alts [] _) = []
    go (Ambig [] _) = []
    go (Ambig ps' (Pure f)) = fmap f <$> ps'
    go p = [p]

instance Applicative (Prod r t nodeF) where
  pure = Pure
  {-# INLINE (<*>) #-}
  Terminal t build <*> q = Terminal t $ flip <$> build <*> q
  NonTerminal r build <*> q = NonTerminal r $ flip <$> build <*> q
  Pure f <*> q = f <$> q
  Ambig ps build <*> q = Ambig ps $ flip <$> build <*> q
  Alts ps build <*> q = alts ps $ flip <$> build <*> q
  Many p build <*> q = Many p $ flip <$> build <*> q

instance Alternative (Prod r t nodeF) where
  empty = Alts [] $ pure identity
  p <|> q = alts [p, q] $ pure identity
  many (Alts [] _) = pure []
  many p = Many p $ Pure identity
  some p = (:) <$> p <*> many p

data Grammar r t nodeF a where
  RuleBind :: Prod r t nodeF (nodeF r) -> (r -> Grammar r t nodeF b) -> Grammar r t nodeF b
  FixBind :: (a -> Grammar r t nodeF a) -> (a -> Grammar r t nodeF b) -> Grammar r t nodeF b
  Return :: a -> Grammar r t nodeF a

instance Functor (Grammar r t nodeF) where
  fmap f (RuleBind p cont) = RuleBind p (cont >>> fmap f)
  fmap f (FixBind unfixed build) = FixBind unfixed (build >>> fmap f)
  fmap f (Return x) = Return $ f x

instance Applicative (Grammar r t nodeF) where
  pure = return
  (<*>) = ap

instance Monad (Grammar r t nodeF) where
  return = Return
  RuleBind p cont >>= f = RuleBind p (cont >=> f)
  FixBind unfixed build >>= f = FixBind unfixed (build >=> f)
  Return x >>= f = f x

instance MonadFix (Grammar r t nodeF) where
  mfix f = FixBind f return

rule :: Prod r t nodeF (nodeF r) -> Grammar r t nodeF r
rule p = RuleBind p return

runGrammar :: MonadFix m
           => (Prod r t nodeF (nodeF r) -> m r)
           -> Grammar r t nodeF a -> m a
runGrammar mkRule = \case
  RuleBind p cont -> do
    nt <- mkRule p
    runGrammar mkRule $ cont nt
  Return a -> return a
  FixBind unfixed build -> do
    a <- mfix $ runGrammar mkRule <$> unfixed
    runGrammar mkRule $ build a
