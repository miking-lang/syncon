{-# LANGUAGE ViewPatterns #-}

module Main where

import Prelude ()
import Pre

import Hedgehog.Main (defaultMain)

import qualified ParseTest

main :: IO ()
main = defaultMain
  [ ParseTest.test
  ]
