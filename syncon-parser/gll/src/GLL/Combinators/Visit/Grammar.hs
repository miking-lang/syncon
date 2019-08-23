
module GLL.Combinators.Visit.Grammar where

import Prelude

import GLL.Types.Grammar

import qualified Data.Map as M

type Grammar_Expr t = M.Map Nt [Prod t] -> M.Map Nt [Prod t]

grammar_nterm :: Nt -> [Prod t] -> [Grammar_Expr t] -> Grammar_Expr t
grammar_nterm x alts ps rules
    | x `M.member` rules = rules
    | otherwise = foldr ($) (M.insert x alts rules) $ ps

grammar_apply :: Grammar_Expr t -> Grammar_Expr t
grammar_apply = id

grammar_seq :: Grammar_Expr t -> Grammar_Expr t -> Grammar_Expr t
grammar_seq p q rules =
    let rules1  = q rules
        rules2  = p rules1 in rules2
