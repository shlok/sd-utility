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
    [ testMovingAvg @Double 1e-8 1e-8 "movingAvg (Double)"
    , testMovingAvg @Float  1e-2 1e-2  "movingAvg (Float)"
    , testMovingAvg @Rational  1e-800 1e-800  "movingAvg (Rational)" ]

--------------------------------------------------------------------------------

testMovingAvg :: forall f . (Arbitrary f, Ord f, Show f, Fractional f) => f -> f -> String -> TestTree
testMovingAvg eps0 eps1 desc = testProperty desc $ monadicIO $ do
    Positive (movingAvgCount :: Int) <- pick arbitrary
    let emptyMovingAvg = fromJust $ MovingAvg.empty movingAvgCount
    elements :: [f] <- pick arbitrary
    let movingAvg = foldl' (flip MovingAvg.append) emptyMovingAvg elements
    let elemsLen = length elements
    case elemsLen of
        n | n == 0 || n < movingAvgCount -> return . isNothing $ lookupMA movingAvg
        _ -> return $ areClose eps0 eps1
                               (fromJust $ lookupMA movingAvg)
                               (sum (lastElems movingAvgCount elements) / fromIntegral movingAvgCount)

areClose :: (Ord f, Fractional f) => f -> f -> f -> f -> Bool
areClose eps0 eps1 x y =
    let d = abs (x - y)
    in (x == 0 && d < eps0)
        || (y == 0 && d < eps0)
        || (x /= 0 && y /= 0 && d / x < eps1 && d / y < eps1)

lastElems :: Int -> [a] -> [a]
lastElems n l = drop (length l - n) l

--------------------------------------------------------------------------------
