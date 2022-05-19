{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module SD.Utility.AverageTrueRange.Tests (tests) where

import Data.List (foldl')
import Data.Maybe (fromJust, isNothing)
import qualified SD.Utility.AverageTrueRange as ATR
import SD.Utility.ExpMovingAverage.Tests (naiveEMA)
import Test.QuickCheck (Positive (Positive))
import Test.QuickCheck.Monadic (monadicIO, pick)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (Arbitrary, arbitrary, testProperty)

tests :: [TestTree]
tests =
  [ testATR @Double 1e-8 1e-8 "averageTrueRange (Double)",
    testATR @Float 1e-2 1e-2 "averageTrueRange (Float)",
    testATR @Rational 1e-800 1e-800 "averageTrueRange (Rational)"
  ]

testATR :: (Arbitrary f, Ord f, Show f, Fractional f) => f -> f -> String -> TestTree
testATR eps0 eps1 desc = testProperty desc $
  monadicIO $ do
    Positive (atrCount :: Int) <- pick arbitrary
    let emptyATR = fromJust $ ATR.empty atrCount
    elements :: [(f, f, f)] <- pick arbitrary
    let mAtr = ATR.lookup $ foldl' (flip ATR.append) emptyATR elements
    case length elements of
      n | n == 0 || n <= atrCount -> return . isNothing $ mAtr
      _ ->
        return $
          areClose
            eps0
            eps1
            (fromJust mAtr)
            (naiveATR atrCount elements)

areClose :: (Ord f, Fractional f) => f -> f -> f -> f -> Bool
areClose eps0 eps1 x y =
  let d = abs (x - y)
   in (x == 0 && d < eps0)
        || (y == 0 && d < eps0)
        || (x /= 0 && y /= 0 && d / x < eps1 && d / y < eps1)

naiveATR :: (Fractional f, Ord f) => Int -> [(f, f, f)] -> f
naiveATR atrCount elements =
  let trueRanges =
        snd $
          foldl'
            ( \(mLc, trs) (h, l, c) ->
                case mLc of
                  Nothing -> (Just c, trs)
                  Just lc ->
                    let tr = maximum [h - l, abs $ lc - h, abs $ lc - l]
                     in (Just c, trs ++ [tr])
            )
            (Nothing, [])
            elements
   in naiveEMA False atrCount trueRanges
