--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

--------------------------------------------------------------------------------

module SD.Utility.AverageTrueRange.Tests (tests) where

--------------------------------------------------------------------------------

import SD.Utility.AverageTrueRange (lookupATR)
import qualified SD.Utility.AverageTrueRange as ATR (empty, append)
import SD.Utility.ExpMovingAverage.Tests (naiveEMA)

import Data.List (foldl')
import Data.Maybe (fromJust, isNothing)
import Test.QuickCheck (Positive (Positive))
import Test.QuickCheck.Monadic (monadicIO, pick)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (Arbitrary, arbitrary, testProperty)

--------------------------------------------------------------------------------

tests :: [TestTree]
tests =
    [ testATR @Double 0.01 "averageTrueRange (Double)"
    , testATR @Float  0.1  "averageTrueRange (Float)" ]

--------------------------------------------------------------------------------

testATR :: (Arbitrary f, Ord f, Show f, Fractional f) => f -> String -> TestTree
testATR eps desc = testProperty desc $ monadicIO $ do
    Positive (atrCount :: Int) <- pick arbitrary
    let emptyATR = fromJust $ ATR.empty atrCount
    elements :: [(f, f, f)] <- pick arbitrary
    let mAtr = lookupATR $ foldl' (flip ATR.append) emptyATR elements
    case length elements of
        n | n == 0 || n <= atrCount -> return . isNothing $ mAtr
        _ -> return $ areClose eps
                               (fromJust $ mAtr)
                               (naiveATR atrCount elements)

areClose :: (Num a, Ord a) => a -> a -> a -> Bool
areClose eps x y = abs (x - y) < eps

naiveATR :: (Fractional f, Ord f) => Int -> [(f, f, f)] -> f
naiveATR atrCount elements =
    let trueRanges = snd $ foldl' (\(mLc, trs) (h, l, c) ->
                                       case mLc of
                                           Nothing -> (Just c, trs)
                                           Just lc -> let tr = maximum [h - l, abs $ lc - h, abs $ lc - l]
                                                       in (Just c, trs ++ [tr]))
                                  (Nothing, []) elements
     in naiveEMA atrCount trueRanges

--------------------------------------------------------------------------------
