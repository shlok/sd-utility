{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module SD.Utility.Percentile.Tests (tests) where

import Data.List (foldl')
import Data.Maybe (fromJust, isNothing)
import qualified SD.Utility.Percentile as Perc
import Test.QuickCheck (Positive (Positive))
import Test.QuickCheck.Monadic (monadicIO, pick)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (Arbitrary, arbitrary, testProperty)

tests :: [TestTree]
tests =
  [ testPercentile @Double 1e-8 1e-8 "percentile (Double)",
    testPercentile @Float 1e-2 1e-2 "percentile (Float)",
    testPercentile @Rational 1e-800 1e-800 "percentile (Rational)"
  ]

testPercentile ::
  forall f.
  (Arbitrary f, Ord f, Show f, Fractional f) =>
  f ->
  f ->
  String ->
  TestTree
testPercentile eps0 eps1 desc = testProperty desc $
  monadicIO $ do
    Positive (percCount :: Int) <- pick arbitrary
    let emptyPerc = fromJust $ Perc.empty percCount
    elements :: [f] <- pick arbitrary
    let perc = foldl' (flip Perc.append) emptyPerc elements
    let elemsLen = length elements
    case elemsLen of
      n | n < percCount -> return . isNothing $ Perc.lookup @f perc
      _ ->
        return $
          areClose
            eps0
            eps1
            (fromJust $ Perc.lookup perc)
            ( (fromIntegral . length . filter (< last elements) $ lastElems percCount elements)
                / fromIntegral percCount
            )

areClose :: (Ord f, Fractional f) => f -> f -> f -> f -> Bool
areClose eps0 eps1 x y =
  let d = abs (x - y)
   in (x == 0 && d < eps0)
        || (y == 0 && d < eps0)
        || (x /= 0 && y /= 0 && d / x < eps1 && d / y < eps1)

lastElems :: Int -> [a] -> [a]
lastElems n l = drop (length l - n) l
