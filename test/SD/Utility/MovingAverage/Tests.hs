--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

--------------------------------------------------------------------------------

module SD.Utility.MovingAverage.Tests (tests) where

--------------------------------------------------------------------------------

import SD.Utility.MovingAverage (lookupMA)
import qualified SD.Utility.MovingAverage as MovingAvg (empty, append)

import Data.List (foldl')
import Data.Maybe (fromJust, isNothing)
import Test.QuickCheck (Positive (Positive))
import Test.QuickCheck.Monadic (monadicIO, pick)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (Arbitrary, arbitrary, testProperty)

--------------------------------------------------------------------------------

tests :: [TestTree]
tests =
    [ testMovingAvg @Double 0.01 "movingAvg (Double)"
    , testMovingAvg @Float  0.1  "movingAvg (Float)" ]

--------------------------------------------------------------------------------

testMovingAvg :: forall f . (Arbitrary f, Ord f, Show f, Fractional f) => f -> String -> TestTree 
testMovingAvg eps desc = testProperty desc $ monadicIO $ do
    Positive (movingAvgCount :: Int) <- pick arbitrary
    let emptyMovingAvg = fromJust $ MovingAvg.empty movingAvgCount
    elements :: [f] <- pick arbitrary
    let movingAvg = foldl' (flip MovingAvg.append) emptyMovingAvg elements
    let elemsLen = length elements
    case elemsLen of
        n | n == 0 || n < movingAvgCount -> return . isNothing $ lookupMA movingAvg
        _ -> return $ areClose eps
                               (fromJust $ lookupMA movingAvg)
                               (sum (lastElems movingAvgCount elements) / fromIntegral movingAvgCount)

areClose :: (Num a, Ord a) => a -> a -> a -> Bool
areClose eps x y = abs (x - y) < eps

lastElems :: Int -> [a] -> [a]
lastElems n l = drop (length l - n) l

--------------------------------------------------------------------------------
