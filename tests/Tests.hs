module Main (main) where

import Prelude
import Test.HUnit
import System.Exit (exitSuccess, exitFailure)

import qualified ParseTests
import qualified QueryTests

main :: IO ()
main = do
  c <- runTestTT tests
  if errors c + failures c == 0
    then exitSuccess
    else exitFailure

  where
    tests = ParseTests.testsAll ++ QueryTests.testsAll
