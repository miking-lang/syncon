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
  Terminal :: !Text -> !(t -> Bool) -> !(Prod r t nodeF (t -> b)) -> Prod r t nodeF b
  NonTerminal :: !r -> !(Prod r t nodeF (r -> a)) -> Prod r t nodeF a
  Pure :: a -> Prod r t nodeF a
  Alts :: ![Prod r t nodeF a] -> !(Prod r t nodeF (a -> b)) -> Prod r t nodeF b
  Many :: !(Prod r t nodeF a) -> !(Prod r t nodeF ([a] -> b)) -> Prod r t nodeF b

-- | Match a token for which the given predicate returns @Just a@,
-- and return the @a@.
terminal :: Text -> (t -> Bool) -> Prod r t nodeF t
terminal label p = Terminal label p $ Pure identity

instance Functor (Prod r t nodeF) where
  {-# INLINE fmap #-}
  fmap f (Terminal label t build) = Terminal label t $ (>>> f) <$> build
  fmap f (NonTerminal r build) = NonTerminal r $ (>>> f) <$> build
  fmap f (Pure a) = Pure $ f a
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

instance Applicative (Prod r t nodeF) where
  pure = Pure
  {-# INLINE (<*>) #-}
  Terminal label t build <*> q = Terminal label t $ flip <$> build <*> q
  NonTerminal r build <*> q = NonTerminal r $ flip <$> build <*> q
  Pure f <*> q = f <$> q
  Alts ps build <*> q = alts ps $ flip <$> build <*> q
  Many p build <*> q = Many p $ flip <$> build <*> q

instance Alternative (Prod r t nodeF) where
  empty = Alts [] $ pure identity
  p <|> q = alts [p, q] $ pure identity
  many (Alts [] _) = pure []
  many p = Many p $ Pure identity
  some p = (:) <$> p <*> many p

data Grammar r t nodeF a where
  AmbigBind :: [Prod r t nodeF r] ->  (Prod r t nodeF r -> Grammar r t nodeF b) -> Grammar r t nodeF b
  RuleBind :: Prod r t nodeF (nodeF r) -> (Prod r t nodeF r -> Grammar r t nodeF b) -> Grammar r t nodeF b
  FixBind :: (a -> Grammar r t nodeF a) -> (a -> Grammar r t nodeF b) -> Grammar r t nodeF b
  Return :: a -> Grammar r t nodeF a

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

rule :: Prod r t nodeF (nodeF r) -> Grammar r t nodeF (Prod r t nodeF r)
rule p = RuleBind p return

-- | Merge these alteratives into a single ambiguous node.
ambig :: [Prod r t nodeF r] -> Grammar r t nodeF (Prod r t nodeF r)
ambig ps = AmbigBind ps return

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
