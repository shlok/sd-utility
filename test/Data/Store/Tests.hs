--------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------

module Data.Store.Tests (tests) where

--------------------------------------------------------------------------------

import Data.ByteString (ByteString, concat)
import Data.Serialize.IEEE754 (putFloat64be, putFloat64le)
import Data.Serialize.Put (runPut)
import Data.Store (Store, encode)
import GHC.Generics (Generic)
import Prelude hiding (concat)
import System.Endian (Endianness (BigEndian, LittleEndian), getSystemEndianness)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

--------------------------------------------------------------------------------

tests :: [TestTree]
tests =
    [ testDoublesEncoding ]

--------------------------------------------------------------------------------

-- Data.Store is supposed to encode a data type containing only Doubles as simply
-- a concatenation of each of the Doubles encoded in machine representation.
-- We test this property here.

data Doubles
   = Doubles { _d1 :: !Double
             , _d2 :: !Double
             , _d3 :: !Double
             , _d4 :: !Double
             , _d5 :: !Double
             } deriving (Generic)

instance Store Doubles

testDoublesEncoding :: TestTree
testDoublesEncoding = testProperty "Encode data type with multiple Doubles" $
    \d1 d2 d3 d4 d5 ->
        encode (Doubles d1 d2 d3 d4 d5) == concat (map encodeDouble [d1, d2, d3, d4, d5])

-- | Encode a single 'Double' in machine representation using the cereal library.
encodeDouble :: Double -> ByteString
encodeDouble = runPut .
        case getSystemEndianness of
            BigEndian -> putFloat64be
            LittleEndian -> putFloat64le

--------------------------------------------------------------------------------
