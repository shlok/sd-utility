--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

--------------------------------------------------------------------------------

module SD.Utility.Exception.Tests (tests) where

--------------------------------------------------------------------------------

import SD.Utility.Exception

import Control.Exception (ArithException (Overflow), SomeException, throw, try)
import Data.Either (lefts)
import System.IO (readFile)
import System.IO.Error (isDoesNotExistError)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, testCase)

--------------------------------------------------------------------------------

tests :: [TestTree]
tests =
    [ testHoldsFor ]

--------------------------------------------------------------------------------

testHoldsFor :: TestTree
testHoldsFor =
    testCase "holdsFor" $ do
        -- Read non-existent file.
        eString <- (try @SomeException) (readFile "3def6a")
        let someException = lefts [eString] !! 0
        assertBool "(1)" $ isDoesNotExistError `holdsFor` someException
        -----
        e <- (try @SomeException) (throw Overflow)
        let someException2 = lefts [e] !! 0
        assertBool "(2)" $ not $ isDoesNotExistError `holdsFor` someException2

--------------------------------------------------------------------------------
