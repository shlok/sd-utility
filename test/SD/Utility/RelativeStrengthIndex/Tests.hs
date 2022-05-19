{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module SD.Utility.RelativeStrengthIndex.Tests (tests) where

import Data.Bifunctor (bimap)
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Maybe (fromJust, isNothing)
import Data.Sequence ((|>))
import qualified Data.Sequence as Seq
import SD.Utility.ExpMovingAverage.Tests (naiveEMA)
import qualified SD.Utility.RelativeStrengthIndex as RSI
import Test.QuickCheck (Positive (Positive))
import Test.QuickCheck.Monadic (monadicIO, pick)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (Arbitrary, arbitrary, testProperty)

tests :: [TestTree]
tests =
  [ testRSI @Double 1e-8 1e-8 "relativeStrengthIndex (Double)",
    testRSI @Float 1e-2 1e-2 "relativeStrengthIndex (Float)",
    testRSI @Rational 1e-800 1e-800 "relativeStrengthIndex (Rational)"
  ]

testRSI :: forall a. (Arbitrary a, Ord a, Show a, Fractional a) => a -> a -> String -> TestTree
testRSI eps0 eps1 desc = testProperty desc $
  monadicIO $ do
    Positive (rsiCount :: Int) <- pick arbitrary
    let emptyRSI = fromJust $ RSI.empty rsiCount
    elements :: [a] <- pick arbitrary
    let mRsi = RSI.lookup $ foldl' (flip RSI.append) emptyRSI elements
    case length elements of
      n | n <= rsiCount -> return . isNothing $ mRsi
      _ ->
        return $
          areClose
            eps0
            eps1
            (fromJust mRsi)
            (naiveRSI rsiCount elements)

areClose :: (Ord a, Fractional a) => a -> a -> a -> a -> Bool
areClose eps0 eps1 x y =
  let d = abs (x - y)
   in (x == 0 && d < eps0)
        || (y == 0 && d < eps0)
        || (x /= 0 && y /= 0 && d / x < eps1 && d / y < eps1)

naiveRSI :: (Fractional a, Ord a) => Int -> [a] -> a
naiveRSI rsiCount elements =
  let (profits, losses) =
        bimap toList toList
          . foldl' (\(accp, accl) (p, l) -> (accp |> p, accl |> l)) (Seq.empty, Seq.empty)
          . map
            ( \x ->
                let profit = max x 0
                    loss = max (- x) 0
                 in (profit, loss)
            )
          $ zipWith (flip (-)) elements (tail elements)
      smmaProfits = naiveEMA True rsiCount profits
      smmaLosses = naiveEMA True rsiCount losses
   in if smmaLosses == 0
        then 100
        else 100 - 100 / (1 + smmaProfits / smmaLosses)
