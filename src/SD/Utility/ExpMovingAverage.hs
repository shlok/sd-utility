-- | This module is intended to be imported qualified: @import qualified SD.Utility.ExpMovingAverage
-- as EMA@.
module SD.Utility.ExpMovingAverage
  ( EMA (),
    empty,
    emptySmoothed,
    append,
    lookup,
  )
where

import Control.Monad (guard)
import Prelude hiding (lookup)

data EMA a = EMA
  { count :: !Int,
    k :: !a,
    currCount :: !Int,
    currEMA :: !a
  }

-- | Initializes an ordinary EMA (with @alpha = 2 / (N + 1)@.) Please provide a positive count.
-- (Returns 'Nothing' otherwise.)
empty :: (Fractional a) => Int -> Maybe (EMA a)
empty = emptyK (\n -> 2 / (fromIntegral n + 1))

-- | Initializes a smoothed moving average (SMMA; i.e., an EMA with @alpha = 1 / N@.) Please provide
-- a positive count. (Returns 'Nothing' otherwise.)
emptySmoothed :: (Fractional a) => Int -> Maybe (EMA a)
emptySmoothed = emptyK (\n -> 1 / fromIntegral n)

-- | Please provide a positive count. (Returns 'Nothing' otherwise.)
emptyK :: (Fractional a) => (Int -> a) -> Int -> Maybe (EMA a)
emptyK kf n
  | n <= 0 = Nothing
  | otherwise =
    Just $
      EMA
        { count = n,
          k = kf n,
          currCount = 0,
          currEMA = 0
        }

append :: (Fractional a) => a -> EMA a -> EMA a
append a m =
  if currCount m < count m
    then
      m
        { currCount = currCount m + 1,
          currEMA = currEMA m + a / fromIntegral (count m)
        }
    else m {currEMA = currEMA m * (1 - k m) + a * k m}

{-# INLINE lookup #-}
lookup :: EMA a -> Maybe a
lookup m = do
  guard $ currCount m == count m
  Just $ currEMA m
