--------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------

module Data.Store.Tests (tests) where

--------------------------------------------------------------------------------

import Data.ByteString (ByteString, concat)
import Data.Serialize.IEEE754 (putFloat32be, putFloat32le, putFloat64be, putFloat64le)
import Data.Serialize.Put (putWord16host, putWord32host, putWord64host, runPut)
import Data.Store (Store, encode)
import Data.Word (Word16, Word32, Word64)
import GHC.Generics (Generic)
import Prelude hiding (concat, encodeFloat)
import System.Endian (Endianness (BigEndian, LittleEndian), getSystemEndianness)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

--------------------------------------------------------------------------------

tests :: [TestTree]
tests =
    [ testNumbersEncoding ]

--------------------------------------------------------------------------------

-- Data.Store is supposed to encode a data type containing only numbers of the standard
-- types as simply a concatenation of each of the numbers encoded in machine representation.
-- We test this property here.

data Numbers
   = Numbers { _n1 :: !Double
             , _n2 :: !Float
             , _n3 :: !Double
             , _n4 :: !Word32
             , _n5 :: !Float
             , _n6 :: !Word64
             , _n7 :: !Word16
             , _n8 :: !Double
             } deriving (Generic)

instance Store Numbers

testNumbersEncoding :: TestTree
testNumbersEncoding = testProperty "Encode data type with multiple numbers" $
    \n1 n2 n3 n4 n5 n6 n7 n8 ->
        encode (Numbers n1 n2 n3 n4 n5 n6 n7 n8)
            == concat [ encodeDouble n1
                      , encodeFloat n2
                      , encodeDouble n3
                      , encodeWord32 n4
                      , encodeFloat n5
                      , encodeWord64 n6
                      , encodeWord16 n7
                      , encodeDouble n8 ]

--------------------------------------------------------------------------------

-- The following functions encode numbers using the cereal library.

encodeWord16 :: Word16 -> ByteString
encodeWord16 = runPut . putWord16host

encodeWord32 :: Word32 -> ByteString
encodeWord32 = runPut . putWord32host

encodeWord64 :: Word64 -> ByteString
encodeWord64 = runPut . putWord64host

encodeDouble :: Double -> ByteString
encodeDouble = runPut . endian putFloat64be putFloat64le

encodeFloat :: Float -> ByteString
encodeFloat = runPut . endian putFloat32be putFloat32le

endian :: a -> a -> a
endian be le =
    case getSystemEndianness of
        BigEndian -> be
        LittleEndian -> le

--------------------------------------------------------------------------------
