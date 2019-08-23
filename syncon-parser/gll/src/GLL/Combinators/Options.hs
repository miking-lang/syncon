module GLL.Combinators.Options where

import Prelude

import Data.Function (on)

-- | CombinatorOptions datatype
--      * left_biased_choice: see function leftBiased
--      * pivot_select: provide a filtering function on `pivots'
data PCOptions = PCOptions  { left_biased_choice    :: Bool
                            , pivot_select          :: Maybe (Int -> Int -> Ordering)
                            , pivot_select_nt       :: Bool
                            , throw_errors          :: Bool
                            , do_memo               :: Bool
                            , max_errors            :: Int
                            , nt_select_test        :: Bool
                            , alt_select_test       :: Bool
                            , seq_select_test       :: Bool
                            }

-- | A list of 'CombinatorOption's for evaluating combinator expressions.
type CombinatorOptions    = [CombinatorOption]

-- | A single option.
type CombinatorOption     = PCOptions -> PCOptions

runOptions :: CombinatorOptions -> PCOptions
runOptions = runOptionsOn defaultOptions

runOptionsOn :: PCOptions -> CombinatorOptions -> PCOptions
runOptionsOn = foldr ($)

-- | The default options: no disambiguation.
defaultOptions :: PCOptions
defaultOptions = PCOptions False Nothing False False False 3 True True True

-- | Enables a 'longest-match' at production level.
maximumPivot :: CombinatorOption
maximumPivot opts = opts {pivot_select = Just compare}

-- | Enables a 'shortest-match' at production level.
minimumPivot :: CombinatorOption
minimumPivot opts = opts {pivot_select = Just (flip compare)}

-- | Discards a pivot select option (internal use only)
anyPivot :: CombinatorOption
anyPivot opts = opts {pivot_select = Nothing}

-- | Enables 'longest-match' at non-terminal level.
maximumPivotAtNt :: CombinatorOption
maximumPivotAtNt opts = opts {pivot_select_nt = True, pivot_select = Just compare}

-- |
-- Set the maximum number of errors shown in case of an unsuccessful parse.
maximumErrors :: Int -> CombinatorOption
maximumErrors n opts = opts { max_errors = n }

-- |
-- If there are no parse results, the default behaviour is to return an empty list.
-- If this option is used, a runtime error will be reported, with debugging information.
throwErrors :: CombinatorOption
throwErrors opts = opts{throw_errors = True}


-- |
-- Turns all occurrences of '<||>' into a 'left biased' variant:
--  only return results of the second alternate if the first alternate
-- does not have any results.
leftBiased :: CombinatorOption
leftBiased opts = opts { left_biased_choice = True }

-- |
-- Whether to use unsafe memoisation to speed up the enumeration of parse results.
useMemoisation :: CombinatorOption
useMemoisation opts = opts { do_memo = True }

-- | Filter a list such that the only remaining elements are equal to
-- the maximum element, given an ordering operator.
maximumsWith :: (a -> a -> Ordering) -> [a] -> [a]
maximumsWith compare xs =
    case xs of
    []      -> []
    [x]     -> [x]
    x:xs    -> maxx xs x []
 where  maxx []     x acc = x : acc
        maxx (y:ys) x acc = case y `compare` x of
                            LT -> maxx ys x acc
                            GT -> maxx ys y []
                            EQ -> maxx ys y (x:acc)

-- assumes every sub-list contains only maximums already
maintainWith :: (Eq k) => (k -> k -> Ordering) -> [[(k,a)]] -> [[(k,a)]]
maintainWith compare =
    maintain .
    filter (not . null)
 where  maintain xss =
            let (max,_):_ = maximumsWith (compare `on` fst) $ map head xss
             in (filter ((== max) . fst . head) xss)

-- |
-- Enables select tests at all levels: nonterminal, alternative and slot.
doSelectTest :: CombinatorOption
doSelectTest opts = opts { nt_select_test = True, alt_select_test = True
                         , seq_select_test = True }

-- |
-- Disables select tests at all levels: nonterminal, alternative and slot.
noSelectTest :: CombinatorOption
noSelectTest opts = opts { nt_select_test = False, alt_select_test = False
                         , seq_select_test = False }

-- |
-- Enables select tests at the level of alternatives
doAltSelectTest :: CombinatorOption
doAltSelectTest opts = opts { alt_select_test = True }

-- |
-- Disables select tests at the level of alternatives
noAltSelectTest :: CombinatorOption
noAltSelectTest opts = opts { alt_select_test = False }

-- |
-- Enables select tests at the level of nonterminals
doNtSelectTest :: CombinatorOption
doNtSelectTest opts = opts { nt_select_test = True }

-- |
-- Disables select tests at the level of nonterminals
noNtSelectTest :: CombinatorOption
noNtSelectTest opts = opts { nt_select_test = False }

-- |
-- Enables select tests at the level of grammar slots
doSlotSelectTest :: CombinatorOption
doSlotSelectTest opts = opts { seq_select_test = True }

-- |
-- Disables select tests at the level of grammar slots
noSlotSelectTest :: CombinatorOption
noSlotSelectTest opts = opts { seq_select_test = False }
