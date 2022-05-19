-- | This module is intended to be imported qualified: @import qualified SD.Utility.MovingAverage
-- as MA@.
module SD.Utility.MovingAverage
  ( MA (),
    empty,
    append,
    lookup,
  )
where

import Control.Monad (guard)
import Data.Maybe (fromJust)
import SD.Utility.MaxLengthSequence (MLSeq, full)
import qualified SD.Utility.MaxLengthSequence as MLSeq (append, empty, head)
import Prelude hiding (lookup)

data MA a = MovingAvg
  { count :: !Int,
    mlSeq :: !(MLSeq a),
    currMA :: !a
  }

-- | Please provide a positive count. (Returns 'Nothing' otherwise.)
empty :: (Num a) => Int -> Maybe (MA a)
empty n = do
  mls <- MLSeq.empty n
  return $
    MovingAvg
      { count = n,
        mlSeq = mls,
        currMA = 0
      }

append :: (Fractional a) => a -> MA a -> MA a
append a m =
  let oldSeq = mlSeq m
      newSeq = MLSeq.append a oldSeq
      denom = fromIntegral (count m)
   in if full oldSeq
        then
          m
            { mlSeq = newSeq,
              currMA = currMA m + (a - fromJust (MLSeq.head oldSeq)) / denom
            }
        else
          m
            { mlSeq = newSeq,
              currMA = currMA m + a / denom
            }

{-# INLINE lookup #-}
lookup :: MA a -> Maybe a
lookup m = do
  guard . full $ mlSeq m
  Just $ currMA m
