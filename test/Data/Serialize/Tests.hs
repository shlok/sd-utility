--------------------------------------------------------------------------------

module Data.Serialize.Tests (tests) where

--------------------------------------------------------------------------------

import Data.Serialize.IEEE754 (putFloat32be, putFloat64be)
import Data.Serialize.Put (Putter, putInt16be, putInt32be, putInt64be, putWord16be, putWord32be, putWord64be, runPut)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (Negative (..), NonNegative (..), testProperty)

--------------------------------------------------------------------------------

tests :: [TestTree]
tests =
    [ testBigEndianOrderWord16
    , testBigEndianOrderWord32
    , testBigEndianOrderWord64
    , testBigEndianOrderInt16NonNeg
    , testBigEndianOrderInt32NonNeg
    , testBigEndianOrderInt64NonNeg
    , testBigEndianOrderInt16Neg
    , testBigEndianOrderInt32Neg
    , testBigEndianOrderInt64Neg
    , testBigEndianOrderFloatNonNeg
    , testBigEndianOrderDoubleNonNeg ]

--------------------------------------------------------------------------------

-- When serializing any of the following in big-endian format, the order is supposed to be preserved.
--     * Unsigned integers.
--     * Nonnegative signed integers.
--     * Negative signed integers.
--     * Nonnegative floating-point numbers.
-- We test this property here.

testBigEndianOrderWord16 :: TestTree
testBigEndianOrderWord16 = testProperty "Big-endian order (Word16)" $
    orderPreserveProp id putWord16be

testBigEndianOrderWord32 :: TestTree
testBigEndianOrderWord32 = testProperty "Big-endian order (Word32)" $
    orderPreserveProp id putWord32be

testBigEndianOrderWord64 :: TestTree
testBigEndianOrderWord64 = testProperty "Big-endian order (Word64)" $
    orderPreserveProp id putWord64be

testBigEndianOrderInt16NonNeg :: TestTree
testBigEndianOrderInt16NonNeg = testProperty "Big-endian order (Int16, nonnegative)" $
    orderPreserveProp getNonNegative putInt16be

testBigEndianOrderInt32NonNeg :: TestTree
testBigEndianOrderInt32NonNeg = testProperty "Big-endian order (Int32, nonnegative)" $
    orderPreserveProp getNonNegative putInt32be

testBigEndianOrderInt64NonNeg :: TestTree
testBigEndianOrderInt64NonNeg = testProperty "Big-endian order (Int64, nonnegative)" $
    orderPreserveProp getNonNegative putInt64be

testBigEndianOrderInt16Neg :: TestTree
testBigEndianOrderInt16Neg = testProperty "Big-endian order (Int16, negative)" $
    orderPreserveProp getNegative putInt16be

testBigEndianOrderInt32Neg :: TestTree
testBigEndianOrderInt32Neg = testProperty "Big-endian order (Int32, negative)" $
    orderPreserveProp getNegative putInt32be

testBigEndianOrderInt64Neg :: TestTree
testBigEndianOrderInt64Neg = testProperty "Big-endian order (Int64, negative)" $
    orderPreserveProp getNegative putInt64be

testBigEndianOrderFloatNonNeg :: TestTree
testBigEndianOrderFloatNonNeg = testProperty "Big-endian order (Float, nonnegative)" $
    orderPreserveProp getNonNegative putFloat32be

testBigEndianOrderDoubleNonNeg :: TestTree
testBigEndianOrderDoubleNonNeg = testProperty "Big-endian order (Double, nonnegative)" $
    orderPreserveProp getNonNegative putFloat64be

orderPreserveProp :: (Ord b) => (a -> b) -> Putter b -> (a -> a -> Bool)
orderPreserveProp unwrap putter =
    \a1 a2 ->
        let put = runPut . putter
            (b1, b2) = (unwrap a1, unwrap a2)
        in b1 `compare` b2 == put b1 `compare` put b2

--------------------------------------------------------------------------------
