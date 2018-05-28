--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

-------------------------------------------------------------------------------

module SD.Utility.MaxLengthSequence.Tests (tests) where

--------------------------------------------------------------------------------

import qualified SD.Utility.MaxLengthSequence as MLSeq

import Data.List (foldl')
import Data.Maybe (fromJust, isNothing)
import Test.QuickCheck (Positive (Positive))
import Test.QuickCheck.Monadic (monadicIO, pick)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (arbitrary, testProperty)

--------------------------------------------------------------------------------

tests :: [TestTree]
tests =
    [ testHead ]

--------------------------------------------------------------------------------

testHead :: TestTree
testHead = testProperty "head" $ monadicIO $ do
    Positive (mlSeqMaxLen :: Int) <- pick arbitrary
    let emptyMlSeq = fromJust $ MLSeq.empty mlSeqMaxLen
    elements :: [Int] <- pick arbitrary
    let mlSeq = foldl' (flip MLSeq.append) emptyMlSeq elements
    case length elements of
        0 -> return . isNothing $ MLSeq.head mlSeq
        n | n <= mlSeqMaxLen -> return $ fromJust (MLSeq.head mlSeq) == head elements
        _ -> return $ fromJust (MLSeq.head mlSeq) == elements !! (length elements - mlSeqMaxLen)

--------------------------------------------------------------------------------
