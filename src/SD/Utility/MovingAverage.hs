--------------------------------------------------------------------------------

module SD.Utility.MovingAverage (
      MovingAvg ()
    , empty
    , append
    , lookupMA
    ) where

--------------------------------------------------------------------------------

import SD.Utility.MaxLengthSequence (MLSeq, full)
import qualified SD.Utility.MaxLengthSequence as MLSeq (append, empty, head)

import Control.Monad (guard)
import Data.Maybe (fromJust)

--------------------------------------------------------------------------------

data MovingAvg a = MovingAvg {
      count  :: !Int
    , mlSeq  :: !(MLSeq a)
    , currMA :: !a
    }

-- | Please provide a positive count. (Returns 'Nothing' otherwise.)
empty :: (Num a) => Int -> Maybe (MovingAvg a)
empty n = do
    mls <- MLSeq.empty n
    return $ MovingAvg { count = n
                       , mlSeq = mls
                       , currMA = 0 }

append :: (Fractional a) => a -> MovingAvg a -> MovingAvg a
append a m =
    let oldSeq = mlSeq m
        newSeq = MLSeq.append a oldSeq
        denom = fromIntegral (count m)
     in if full oldSeq
         then m { mlSeq = newSeq
                , currMA = currMA m + (a - fromJust (MLSeq.head oldSeq)) / denom  }
         else m { mlSeq = newSeq
                , currMA = currMA m + a / denom }

lookupMA :: MovingAvg a -> Maybe a
lookupMA m = do
    guard . full $ mlSeq m
    Just $ currMA m

--------------------------------------------------------------------------------
