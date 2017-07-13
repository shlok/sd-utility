--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

import qualified SD.Utility.IO.Tests
import qualified SD.Utility.Read.Tests
import qualified SD.Utility.Scientific.Tests

import Test.Tasty (TestTree, defaultMain, testGroup)

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Tests"
        [ testGroup "SD.Utility.IO.Tests"
                     SD.Utility.IO.Tests.tests,
          testGroup "SD.Utility.Read.Tests"
                     SD.Utility.Read.Tests.tests,
          testGroup "SD.Utility.Scientific.Tests"
                     SD.Utility.Scientific.Tests.tests ]
