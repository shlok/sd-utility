--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

-------------------------------------------------------------------------------

module SD.Utility.Streaming.Tests (tests) where

--------------------------------------------------------------------------------

import SD.Utility.Streaming

import Data.ByteString (pack, unpack)
import Data.ByteString.Streaming (fromChunks)
import Data.List (intercalate)
import Data.Word (Word8)
import SD.Utility.Random (randomSplitList)
import Streaming.Prelude (Of ((:>)), each, toList)
import Test.QuickCheck.Monadic (monadicIO, pick, run)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (arbitrary, testProperty)

--------------------------------------------------------------------------------

tests :: [TestTree]
tests =
    [ testLines ]

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
