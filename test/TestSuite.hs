--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

import qualified SD.Utility.Calendar.Tests
import qualified SD.Utility.Exception.Tests
import qualified SD.Utility.IO.Tests
import qualified SD.Utility.MaxLengthSequence.Tests
import qualified SD.Utility.MinMaxSequence.Tests
import qualified SD.Utility.MovingAverage.Tests
import qualified SD.Utility.Read.Tests
import qualified SD.Utility.Scientific.Tests
import qualified SD.Utility.Streaming.Tests

import Test.Tasty (TestTree, defaultMain, testGroup)

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Tests"
        [ testGroup "SD.Utility.Calendar.Tests"
                     SD.Utility.Calendar.Tests.tests,
          testGroup "SD.Utility.Exception.Tests"
                     SD.Utility.Exception.Tests.tests,
          testGroup "SD.Utility.IO.Tests"
                     SD.Utility.IO.Tests.tests,
          testGroup "SD.Utility.MaxLengthSequence.Tests"
                     SD.Utility.MaxLengthSequence.Tests.tests,
          testGroup "SD.Utility.MinMaxSequence.Tests"
                     SD.Utility.MinMaxSequence.Tests.tests,
          testGroup "SD.Utility.MovingAverage.Tests"
                     SD.Utility.MovingAverage.Tests.tests,
          testGroup "SD.Utility.Read.Tests"
                     SD.Utility.Read.Tests.tests,
          testGroup "SD.Utility.Scientific.Tests"
                     SD.Utility.Scientific.Tests.tests,
          testGroup "SD.Utility.Streaming.Tests"
                     SD.Utility.Streaming.Tests.tests ]

--------------------------------------------------------------------------------
