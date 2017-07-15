--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

--------------------------------------------------------------------------------

module SD.Utility.Read.Tests (tests) where

--------------------------------------------------------------------------------

import SD.Utility.Read

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)
import Text.ParserCombinators.ReadP (readP_to_S)

--------------------------------------------------------------------------------

tests :: [TestTree]
tests =
    [ testIntegralP ]

--------------------------------------------------------------------------------

testIntegralP :: TestTree
testIntegralP =
    testCase "integralP" $ do
        let parser = readP_to_S (integralP @Int)
        assertEqual "(1)" (parser "123") [(1,"23"), (12, "3"), (123, "")]
        assertEqual "(2)" (parser "12z") [(1,"2z"), (12, "z")]
        assertEqual "(3)" (parser "001") [(0, "01"), (0, "1"), (1, "")]
        assertEqual "(4)" (parser "z12") []

--------------------------------------------------------------------------------
