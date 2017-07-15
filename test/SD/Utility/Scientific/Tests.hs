--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

--------------------------------------------------------------------------------

module SD.Utility.Scientific.Tests (tests) where

--------------------------------------------------------------------------------

import SD.Utility.Scientific

import Data.Maybe (fromJust, isJust, isNothing)
import Data.Scientific (scientific)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Text.ParserCombinators.ReadP (readP_to_S)

--------------------------------------------------------------------------------

tests :: [TestTree]
tests =
    [ testRealFloatP,
      testScientificToRealFloat_1,
      testScientificToRealFloat_2 ]

--------------------------------------------------------------------------------

testRealFloatP :: TestTree
testRealFloatP =
    testCase "realFloatP" $ do
        let parser = readP_to_S (realFloatP @Double)
        assertEqual "(1)" (parser "1.0e3") [(1, "e3"), (1000, "")]
        assertEqual "(2)" (parser "1.0e1000") [(1, "e1000")]

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
