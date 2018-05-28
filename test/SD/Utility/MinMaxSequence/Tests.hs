--------------------------------------------------------------------------------

{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications #-}

-------------------------------------------------------------------------------

module SD.Utility.MinMaxSequence.Tests (tests) where

--------------------------------------------------------------------------------

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
    [ testMinMax @Double "minMax (Double)"
    , testMinMax @Float  "minMax (Float)"
    , testMinMax @Int    "minMax (Int)" ]

--------------------------------------------------------------------------------

testMinMax :: forall f . (Arbitrary f, Ord f, Show f) => String -> TestTree 
testMinMax desc = testProperty desc $ monadicIO $ do
    Positive (mmSeqMaxLen :: Int) <- pick arbitrary
    let emptyMmSeq = fromJust $ MMSeq.empty mmSeqMaxLen
    elements :: [f] <- pick arbitrary
    let mmSeq = foldl' (flip MMSeq.append) emptyMmSeq elements
    let min' = MMSeq.lookupMin mmSeq
    let max' = MMSeq.lookupMax mmSeq
    let fullMin = MMSeq.lookupFullMin mmSeq
    let fullMax = MMSeq.lookupFullMax mmSeq
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
