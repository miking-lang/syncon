{-# LANGUAGE ViewPatterns #-}

module Main where

import Prelude ()
import Pre

import Hedgehog.Main (defaultMain)

import qualified ParseTest
import qualified AutomataTest

main :: IO ()
main = defaultMain
  [ AutomataTest.test
  , ParseTest.test
  ]
