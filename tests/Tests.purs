module Main where

import Data.Array
import Data.Maybe

import Debug.Trace
import Control.Monad.Eff

-- Import the library's module(s)
import Starter.Kit.Example

-- Import Test.QuickCheck, which supports property-based testing
import Test.QuickCheck

-- Main.main is the entry point of the application
--
-- In the case of the test suite, Main.main will use QuickCheck to test a collection
-- of properties that we expect of the diffs function.
main = do

  -- Use quickCheck' to override the number of tests to perform.
  -- In this case, we only need to run the test once, since there is
  -- only one empty list.
  
  trace "The differences of an empty list are empty."
  quickCheck' 1 $ diffs [] == Just []

  trace "The differences of a single-element list are empty."
  quickCheck $ \n -> diffs [n] == Just []

  trace "The differences of a pair of equal elements are zero."
  quickCheck $ \n -> diffs [n, n] == Just [0]

  trace "The diffs function returns Just (...) for a sorted list."
  quickCheck $ \ns -> isJust $ diffs $ sort ns

  trace "The diffs function returns Nothing for a reverse-sorted list with \
        \at least one pair of unequal elements."
  quickCheck $ \n1 ns -> isNothing $ diffs $ reverse $ sort (n1 : (n1 + 1) : ns)

