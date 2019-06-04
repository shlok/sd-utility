--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------

module SD.Utility.Streaming.Tests (tests) where

--------------------------------------------------------------------------------

import SD.Utility.Streaming (slidingWindow')

import Data.Sequence (fromList, index, singleton)
import qualified Data.Sequence as Seq (length)
import Streaming.Prelude (each, toList_, yield)
import Test.QuickCheck (NonEmptyList (NonEmpty), Positive (Positive))
import Test.QuickCheck.Monadic (monadicIO, pick, run)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.QuickCheck (arbitrary, testProperty)

--------------------------------------------------------------------------------

tests :: [TestTree]
tests =
    [ testSlidingWindow_1
    , testSlidingWindow_2
    , testSlidingWindow_3
    , testSlidingWindow_4
    , testSlidingWindow_5 ]

--------------------------------------------------------------------------------

testSlidingWindow_1 :: TestTree
testSlidingWindow_1 = testProperty "slidingWindow (1)" $ monadicIO $ do
    -- A sliding window on an empty stream should always be empty as well.
    n :: Int <- pick arbitrary
    seqs <- run . toList_ $ slidingWindow' n (each [])
    return $ length seqs == 0

testSlidingWindow_2 :: TestTree
testSlidingWindow_2 = testProperty "slidingWindow (2)" $ monadicIO $ do
    -- A sliding window on a non-empty finite stream should always result in the same number of sequences.
    n :: Int <- pick arbitrary
    NonEmpty (list :: [Double]) <- pick arbitrary
    seqs <- run . toList_ $ slidingWindow' n (each list)
    return $ length seqs == length list

testSlidingWindow_3 :: TestTree
testSlidingWindow_3 = testProperty "slidingWindow (3)" $ monadicIO $ do
    -- A sliding window on a singleton stream should always result in the very same singleton.
    (n, a) :: (Int, Double) <- pick arbitrary
    seqs <- run . toList_ $ slidingWindow' n (yield a)
    return $ length seqs == 1 && Seq.length (seqs !! 0) == 1 && (seqs !! 0) `index` 0 == a

testSlidingWindow_4 :: TestTree
testSlidingWindow_4 = testProperty "slidingWindow (4)" $ monadicIO $ do
    -- A sliding window of length 1 should always give us back the same elements.
    Positive (x :: Int) <- pick arbitrary
    let n = negate x + 2 -- n < 2. (Lengths less than 1 should be treated as 1.)
    list :: [Double] <- pick arbitrary
    seqs <- run . toList_ $ slidingWindow' n (each list)
    return $ length list == length seqs
                 && all (\(a, sequ) -> Seq.length sequ == 1 && a == sequ `index` 0) (zip list seqs)

testSlidingWindow_5 :: TestTree
testSlidingWindow_5 =
    testCase "slidingWindow (5)" $ do
        let list :: [Double] = [1, 2, 3, 4]

        -- Sequences resulting from various window lengths.
        [seqs1, seqs2, seqs3, seqs4, seqs5, seqs6, seqs60]
            <- mapM toList_ $ map ((flip slidingWindow') (each list)) [1, 2, 3, 4, 5, 6, 60]

        -- Expected sequences.
        let seqs1e = map singleton [1, 2, 3, 4]
        let seqs2e = [ fromList [1, 2], fromList [2, 3], fromList [3, 4], singleton 4]
        let seqs3e = [ fromList [1, 2, 3], fromList [2, 3, 4], fromList [3, 4], singleton 4]
        let seqs4e = [ fromList [1, 2, 3, 4], fromList [2, 3, 4], fromList [3, 4], singleton 4 ]

        assertEqual "" seqs1 seqs1e
        assertEqual "" seqs2 seqs2e
        assertEqual "" seqs3 seqs3e
        assertEqual "" seqs4 seqs4e
        assertEqual "" seqs5 seqs4e
        assertEqual "" seqs6 seqs4e
        assertEqual "" seqs60 seqs4e

--------------------------------------------------------------------------------
