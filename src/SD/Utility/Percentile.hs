{-# LANGUAGE NamedFieldPuns #-}

-- | This module is intended to be imported qualified: @import qualified SD.Utility.Percentile as
-- Perc@.
module SD.Utility.Percentile (Percentile (), empty, append, lookup) where

import Control.Monad
import Data.Maybe
import Data.OSTree (OSTree)
import qualified Data.OSTree as OT
import SD.Utility.MaxLengthSequence (MLSeq)
import qualified SD.Utility.MaxLengthSequence as MLSeq
import Prelude hiding (lookup)

-- | A data structure that maintains a certain (“count”) number of items and allows us to query the
-- “percentile” of the latest item, i.e., the number of items less than the latest item divided by
-- the count. (Original motivation: Implied volatility (IV) percentile.)
data Percentile a = Percentile
  { -- | Needed for finding the oldest element for deletion, as well as the latest element for
    -- querying.
    mlSeq :: !(MLSeq a),
    tree :: !(OSTree a)
  }

-- | Please provide a positive count; returns 'Nothing' otherwise.
empty :: Int -> Maybe (Percentile a)
empty count = do
  guard $ count > 0
  return $
    Percentile
      { mlSeq = fromJust $ MLSeq.empty count,
        tree = OT.empty
      }

append :: (Ord a) => a -> Percentile a -> Percentile a
append a perc@Percentile {mlSeq, tree} =
  perc
    { mlSeq = MLSeq.append a mlSeq,
      tree =
        OT.insert a $
          if MLSeq.full mlSeq then OT.delete (fromJust $ MLSeq.head mlSeq) tree else tree
    }

-- | Obtains the “percentile” of the latest item; see 'Percentile'. Returns 'Nothing' if the
-- 'Percentile' hasn’t yet reached its full count.
lookup :: (Fractional f, Ord a) => Percentile a -> Maybe f
lookup Percentile {mlSeq, tree} =
  case MLSeq.tail mlSeq of
    Just a' | MLSeq.full mlSeq ->
      case OT.rank tree a' of
        Nothing -> error $ errStr "lookup; unexpected rank"
        Just r -> Just $ fromIntegral r / fromIntegral (MLSeq.length mlSeq)
    _ -> Nothing

errStr :: String -> String
errStr = ("SD.Utility.Percentile: " ++)
