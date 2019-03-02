--------------------------------------------------------------------------------

module Data.Serialize.Tests (tests) where

--------------------------------------------------------------------------------

import Data.Serialize.Put (putWord16be, putWord32be, putWord64be, runPut)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

--------------------------------------------------------------------------------

tests :: [TestTree]
tests =
    [ testBigEndianOrderWord16
    , testBigEndianOrderWord32
    , testBigEndianOrderWord64 ]

--------------------------------------------------------------------------------

-- A nice property of serializing unsigned integers in big-endian format is that order is preserved.
-- We test this property here.

testBigEndianOrderWord16 :: TestTree
testBigEndianOrderWord16 = testProperty "Big-endian order (Word16)" $
    \w1 w2 -> w1 `compare` w2 == put w1 `compare` put w2
        where put = runPut . putWord16be

testBigEndianOrderWord32 :: TestTree
testBigEndianOrderWord32 = testProperty "Big-endian order (Word32)" $
    \w1 w2 -> w1 `compare` w2 == put w1 `compare` put w2
        where put = runPut . putWord32be

testBigEndianOrderWord64 :: TestTree
testBigEndianOrderWord64 = testProperty "Big-endian order (Word64)" $
    \w1 w2 -> w1 `compare` w2 == put w1 `compare` put w2
        where put = runPut . putWord64be

--------------------------------------------------------------------------------
