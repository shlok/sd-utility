--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------

module SD.Utility.IO.Tests (tests) where

--------------------------------------------------------------------------------

import SD.Utility.IO    (hClose, openFile)

import           Control.Exception (Exception (fromException), IOException)
import           Data.Either       (isLeft, isRight, lefts, rights)
import           Data.Maybe        (fromJust, isJust)
import           System.IO         (IOMode (ReadMode))
import qualified System.IO         as S (hClose, openFile)
import           System.IO.Error   (isDoesNotExistError)
import           Test.Tasty        (TestTree)
import           Test.Tasty.HUnit  (assertBool, testCase)

--------------------------------------------------------------------------------

tests :: [TestTree]
tests =
    [ testOpenFile_1,
      testOpenFile_2,
      testHClose ]

--------------------------------------------------------------------------------

testOpenFile_1 :: TestTree
testOpenFile_1 =
    testCase "testOpenFile (1)" $ do
        let fileDir = "test/SD/Utility/IO/"
        eHandle <- openFile (fileDir ++ "empty.txt") ReadMode
        assertBool "Expected Right handle" $ isRight eHandle
        S.hClose $ (rights [eHandle]) !! 0


testOpenFile_2 :: TestTree
testOpenFile_2 =
    testCase "testOpenFile (2)" $ do
        -- Try to read non-existent file.
        eHandle <- openFile "d70893fbc4824f6b" ReadMode
        assertBool "Expected Left" $ isLeft eHandle
        let someException = lefts [eHandle] !! 0
        let mIOException :: Maybe IOException = fromException someException
        assertBool "Expected IOException" $ isJust mIOException
        let ioException = fromJust mIOException
        assertBool "Expected isDoesNotExistError" $
            isDoesNotExistError ioException

--------------------------------------------------------------------------------

testHClose :: TestTree
testHClose =
    testCase "testHClose" $ do
        let fileDir = "test/SD/Utility/IO/"
        handle <- S.openFile (fileDir ++ "empty.txt") ReadMode
        eEmptyTuple <- hClose handle
        assertBool "Expected Right" $ isRight eEmptyTuple
        assertBool "Expected empty tuple" $ (rights [eEmptyTuple]) !! 0 == ()

--------------------------------------------------------------------------------
