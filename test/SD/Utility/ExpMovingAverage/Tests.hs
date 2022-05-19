{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module SD.Utility.ExpMovingAverage.Tests
  ( tests,
    naiveEMA,
  )
where

import Data.List (foldl')
import Data.Maybe (fromJust, isNothing)
import qualified SD.Utility.ExpMovingAverage as EMA
import Test.QuickCheck (Positive (Positive))
import Test.QuickCheck.Monadic (monadicIO, pick)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (Arbitrary, arbitrary, testProperty)

tests :: [TestTree]
tests =
  [ testEMA @Double 1e-8 1e-8 "expMovingAvg (Double)",
    testEMA @Float 1e-2 1e-2 "expMovingAvg (Float)",
    testEMA @Rational 1e-800 1e-800 "expMovingAvg (Rational)"
  ]

testEMA :: (Arbitrary f, Ord f, Show f, Fractional f) => f -> f -> String -> TestTree
testEMA eps0 eps1 desc = testProperty desc $
  monadicIO $ do
    Positive (emaCount :: Int) <- pick arbitrary
    let emptyEMA = fromJust $ EMA.empty emaCount
    elements :: [f] <- pick arbitrary
    let mEma = EMA.lookup $ foldl' (flip EMA.append) emptyEMA elements
    case length elements of
      n | n == 0 || n < emaCount -> return . isNothing $ mEma
      _ ->
        return $
          areClose
            eps0
            eps1
            (fromJust mEma)
            (naiveEMA emaCount elements)

areClose :: (Ord f, Fractional f) => f -> f -> f -> f -> Bool
areClose eps0 eps1 x y =
  let d = abs (x - y)
   in (x == 0 && d < eps0)
        || (y == 0 && d < eps0)
        || (x /= 0 && y /= 0 && d / x < eps1 && d / y < eps1)

naiveEMA :: (Fractional f) => Int -> [f] -> f
naiveEMA emaCount elements =
  let (firstElems, otherElems) = splitAt emaCount elements
      ma = sum firstElems / fromIntegral emaCount
      k = 2 / (fromIntegral emaCount + 1)
   in foldl' (\acc x -> acc * (1 - k) + x * k) ma otherElems
