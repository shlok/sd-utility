--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

--------------------------------------------------------------------------------

module SD.Utility.Scientific.Tests (tests) where

--------------------------------------------------------------------------------

import SD.Utility.Scientific

import Data.Maybe (fromJust, isJust, isNothing)
import Data.Scientific (scientific)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, testCase)

--------------------------------------------------------------------------------

tests :: [TestTree]
tests =
    [ testScientificToRealFloat_1,
      testScientificToRealFloat_2 ]

--------------------------------------------------------------------------------

testScientificToRealFloat_1 :: TestTree
testScientificToRealFloat_1 =
    testCase "scientificToRealFloat (1)" $ do
        let sc = scientific 10 (-3)
        let mDouble = scientificToRealFloat @Double sc
        assertBool "Expected Just" $ isJust mDouble
        let d = fromJust mDouble
        assertBool "Expected 0.01" $ abs (d - 0.01) < 1e-6

testScientificToRealFloat_2 :: TestTree
testScientificToRealFloat_2 =
    testCase "scientificToRealFloat (2)" $ do
        let sc = scientific 1 10000 -- Too large for Double.
        let mDouble = scientificToRealFloat @Double sc
        assertBool "Expected Nothing" $ isNothing mDouble

--------------------------------------------------------------------------------
