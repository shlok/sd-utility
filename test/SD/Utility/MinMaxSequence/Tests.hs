--------------------------------------------------------------------------------

{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications #-}

--------------------------------------------------------------------------------

module SD.Utility.MinMaxSequence.Tests (tests) where

--------------------------------------------------------------------------------

import SD.Utility.MinMaxSequence (MMSeqSetting (..))
import qualified SD.Utility.MinMaxSequence as MMSeq

import Data.List (foldl')
import Data.Maybe (fromJust, isNothing)
import Test.QuickCheck (Positive (Positive))
import Test.QuickCheck.Monadic (monadicIO, pick)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (Arbitrary, arbitrary, testProperty)

--------------------------------------------------------------------------------

tests :: [TestTree]
tests =
    [ testMinMax @Double   "minMax (Double)"
    , testMinMax @Float    "minMax (Float)"
    , testMinMax @Int      "minMax (Int)"
    , testMinMax @Integer  "minMax (Integer)"
    , testMinMax @Rational "minMax (Rational)"
    , testMinMax @String   "minMax (String)" ]

--------------------------------------------------------------------------------

testMinMax :: forall f . (Arbitrary f, Ord f, Show f) => String -> TestTree
testMinMax desc = testProperty desc $ monadicIO $ do
    Positive (mmSeqMaxLen :: Int) <- pick arbitrary
    let emptyMmSeqMin = fromJust $ MMSeq.empty mmSeqMaxLen MMSeqMin
    let emptyMmSeqMax = fromJust $ MMSeq.empty mmSeqMaxLen MMSeqMax
    elements :: [f] <- pick arbitrary
    let mmSeqMin = foldl' (flip MMSeq.append) emptyMmSeqMin elements
    let mmSeqMax = foldl' (flip MMSeq.append) emptyMmSeqMax elements
    let min' = MMSeq.lookupValue mmSeqMin
    let max' = MMSeq.lookupValue mmSeqMax
    let fullMin = MMSeq.lookupFullValue mmSeqMin
    let fullMax = MMSeq.lookupFullValue mmSeqMax
    case length elements of
        0 -> return $ all isNothing [min', max', fullMin, fullMax]
        n | n < mmSeqMaxLen -> return $    fromJust min' == minimum elements
                                        && fromJust max' == maximum elements
                                        && isNothing fullMin
                                        && isNothing fullMax
        _ -> let remElems = drop (length elements - mmSeqMaxLen) elements
                 elemsMin = minimum remElems
                 elemsMax = maximum remElems
              in return $    fromJust min' == elemsMin
                          && fromJust max' == elemsMax
                          && fromJust fullMin == elemsMin
                          && fromJust fullMax == elemsMax

--------------------------------------------------------------------------------
