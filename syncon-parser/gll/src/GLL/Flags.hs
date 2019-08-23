
module GLL.Flags where

import Prelude

-- | Flags to influence the behaviour of the parser.
data Flags   = Flags    { symbol_nodes          :: Bool
                        , intermediate_nodes    :: Bool
                        , edges                 :: Bool
                        , flexible_binarisation :: Bool
                        , max_errors            :: Int
                        , do_select_test        :: Bool
                        }

-- | The default flags:
-- * Do not add symbol nodes to the 'SPPF'.
-- * Do not add intermediate nodes to the 'SPPF'.
-- * Do not add edges to the 'SPPF'.
-- * Flexible binarisation.
-- * The three furthest discoveries of a token mismatch are reported.
-- * Select tests are performed.
defaultFlags = Flags False False False True 3 True

-- | Execute the given 'Options' in left-to-right order on 'defaultFlags'.
runOptions :: ParseOptions -> Flags
runOptions = foldr ($) defaultFlags

-- | An option updates the current set of 'Flags'.
type ParseOption = Flags -> Flags

-- | A list of 'ParserOption's
type ParseOptions = [ParseOption]

-- |
-- Create the 'SPPF' with all nodes and edges, not necessarily strictly binarised.
fullSPPF :: ParseOption
fullSPPF flags = flags{symbol_nodes = True, intermediate_nodes = True, edges = True}

-- |
-- Create all nodes, but no edges between nodes.
allNodes :: ParseOption
allNodes flags = flags{symbol_nodes = True, intermediate_nodes = True}

-- |
-- Create packed-nodes only.
packedNodesOnly :: ParseOption
packedNodesOnly flags = flags{symbol_nodes = False, intermediate_nodes = False, edges = False}

-- |
-- Fully binarise the SPPF, resulting in a larger 'SPPF' and possibly slower runtimes.
-- When this flag is on, packed nodes can only have a single symbol node child
-- or one intermediate node child and one symbol node child.
-- With the flag disabled a packed node can have two symbol node children.
strictBinarisation :: ParseOption
strictBinarisation flags = flags{flexible_binarisation = False}

-- |
-- Set the maximum number of errors shown in case of an unsuccessful parse.
maximumErrors :: Int -> ParseOption
maximumErrors n flags = flags {max_errors = n}

-- |
-- Turn of select tests. Disables lookahead.
noSelectTest :: ParseOption
noSelectTest flags = flags{do_select_test = False}
