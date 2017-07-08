--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

import qualified SD.Utility.IO.Tests

import Test.Tasty (TestTree, defaultMain, testGroup)

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Tests"
        [ testGroup "SD.Utility.IO.Tests"
                     SD.Utility.IO.Tests.tests ]
