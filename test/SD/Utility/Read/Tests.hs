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
        assertEqual "(1)" (readP_to_S (integralP @Int) "123") $
            [(1,"23"), (12, "3"), (123, "")]
        assertEqual "(2)" (readP_to_S (integralP @Int) "12z") $
            [(1,"2z"), (12, "z")]
        assertEqual "(3)" (readP_to_S (integralP @Int) "z12") []

--------------------------------------------------------------------------------
