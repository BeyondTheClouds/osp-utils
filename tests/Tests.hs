module Main (main) where

import Prelude
import Test.HUnit
import System.Exit (exitSuccess, exitFailure)

import qualified ParseTests
import qualified QueryTests

-- Remember to M-x haskell-session-change-target and choose tests or
-- osp-utils.

main :: IO ()
main = do
  c <- runTestTT tests
  if errors c + failures c == 0
    then exitSuccess
    else exitFailure

  where
    tests :: Test
    tests = TestList [ ParseTests.testsAll
                     , QueryTests.testsAll ]
