--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

-------------------------------------------------------------------------------

module SD.Utility.Streaming.Tests (tests) where

--------------------------------------------------------------------------------

import SD.Utility.Streaming

import Control.Monad.Except (ExceptT, runExceptT)
import Data.ByteString (pack, unpack)
import Data.ByteString.Streaming (fromChunks)
import Data.List (intercalate)
import Data.Sequence (fromList, index, singleton)
import qualified Data.Sequence as Seq (length)
import Data.Word (Word8)
import SD.Utility.Random (randomSplitList)
import Streaming.Prelude (Of ((:>)), Stream, each, toList, toList_, yield)
import Test.QuickCheck (NonEmptyList (NonEmpty))
import Test.QuickCheck.Monadic (monadicIO, pick, run)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.QuickCheck (arbitrary, testProperty)

--------------------------------------------------------------------------------

tests :: [TestTree]
tests =
    [ testLines
    , testMapWhile
    , testTakeWhile
    , testSlidingWindow_1
    , testSlidingWindow_2
    , testSlidingWindow_3
    , testSlidingWindow_4 ]

--------------------------------------------------------------------------------

testLines :: TestTree
testLines = testProperty "streamLines" $ monadicIO $ do
    -- Start with a random list of [Word8] lists.
    -- Each [Word8] is supposed to represent a strict bytestring line.
    wordLists :: [[Word8]] <- pick arbitrary
    
    -- Filter newlines (10) out of our lines to make them “real lines.”
    let wordLists' = map (filter (/=10)) wordLists

    -- Join the lines with newlines into a one [Word8], randomly split
    -- it into a [[Word8]], and convert the result to a [ByteString].
    -- The idea is that we now have a random list of strict bytestrings
    -- that are not necessarily separated at the newline boundaries. (The
    -- newlines are buried somewhere within the bytestrings themselves.)
    let joined = intercalate [10] wordLists'
    randomlySplitted <- run $ randomSplitList joined
    let byteStrings = map pack randomlySplitted

    -- Turn the random bytestrings into a streaming bytestring, use
    -- our streamLines function (the one we’re testing) to get our strict
    -- bytestring lines back, and verify that these lines are correct.
    lineByteStrings :> _ <- run . toList . streamLines . fromChunks $ each byteStrings
    let wordLists'' = map unpack lineByteStrings
    return $ if wordLists' == [[]]
              then wordLists'' == [] -- Special case.
              else wordLists' == wordLists''

--------------------------------------------------------------------------------

testMapWhile :: TestTree
testMapWhile =
    testCase "mapWhile" $ do
        let stream = each [1..100]
        let stream' :: Stream (Of Int) (ExceptT String IO) () = mapWhile (*2) (<50) show stream
        let stream'' :: Stream (Of Int) (ExceptT String IO) () = mapWhile (*2) (<500) show stream
        eRes' <- runExceptT $ toList_ stream'
        assertEqual "Expected failure" eRes' (Left "25")
        eRes'' <- runExceptT $ toList_ stream''
        assertEqual "Expected success" eRes'' (Right [2,4..200])

testTakeWhile :: TestTree
testTakeWhile =
    testCase "takeWhile" $ do
        let stream = each [1..100]
        let stream' :: Stream (Of Int) (ExceptT String IO) () = takeWhile' (<50) show stream
        let stream'' :: Stream (Of Int) (ExceptT String IO) () = takeWhile' (<500) show stream
        eRes' <- runExceptT $ toList_ stream'
        assertEqual "Expected failure" eRes' (Left "50")
        eRes'' <- runExceptT $ toList_ stream''
        assertEqual "Expected success" eRes'' (Right [1..100])

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
testSlidingWindow_4 =
    testCase "slidingWindow (4)" $ do
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
