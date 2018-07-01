--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

-------------------------------------------------------------------------------

module SD.Utility.ExpMovingAverage.Tests (tests) where

--------------------------------------------------------------------------------

import SD.Utility.ExpMovingAverage (lookupEMA)
import qualified SD.Utility.ExpMovingAverage as EMA (empty, append)

import Data.List (foldl', splitAt)
import Data.Maybe (fromJust, isNothing)
import Test.QuickCheck (Positive (Positive))
import Test.QuickCheck.Monadic (monadicIO, pick)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (Arbitrary, arbitrary, testProperty)

--------------------------------------------------------------------------------

tests :: [TestTree]
tests =
    [ testEMA @Double 0.01 "expMovingAvg (Double)"
    , testEMA @Float  0.1  "expMovingAvg (Float)" ]

--------------------------------------------------------------------------------

testEMA :: forall f . (Arbitrary f, Ord f, Show f, Fractional f) => f -> String -> TestTree 
testEMA eps desc = testProperty desc $ monadicIO $ do
    Positive (emaCount :: Int) <- pick arbitrary
    let emptyEMA = fromJust $ EMA.empty emaCount
    elements :: [f] <- pick arbitrary
    let ema = foldl' (flip EMA.append) emptyEMA elements
    let elemsLen = length elements
    case elemsLen of
        n | n == 0 || n < emaCount -> return . isNothing $ lookupEMA ema
        _ -> return $ areClose eps
                               (fromJust $ lookupEMA ema)
                               (calculateEma emaCount elements)

areClose :: (Num a, Ord a) => a -> a -> a -> Bool
areClose eps x y = abs (x - y) < eps

calculateEma :: (Fractional f) => Int -> [f] -> f
calculateEma emaCount elements =
    let (firstElems, otherElems) = splitAt emaCount elements
        ma = sum firstElems / fromIntegral emaCount
        k = 2 / (fromIntegral emaCount + 1)
     in foldl' (\acc x -> acc * (1 - k) + x * k) ma otherElems

--------------------------------------------------------------------------------
