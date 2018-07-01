--------------------------------------------------------------------------------

module SD.Utility.ExpMovingAverage (
      ExpMovingAvg ()
    , empty
    , append
    , lookupEMA
    ) where

--------------------------------------------------------------------------------

import Control.Monad (guard)

--------------------------------------------------------------------------------

data ExpMovingAvg a = ExpMovingAvg {
      count     :: !Int
    , k         :: !a
    , currCount :: !Int
    , currEMA   :: !a
    }

-- | Please provide a positive count. (Returns 'Nothing' otherwise.)
empty :: (Fractional a) => Int -> Maybe (ExpMovingAvg a)
empty n
    | n <= 0 = Nothing
    | otherwise = Just $ ExpMovingAvg { count = n
                                      , k = 2 / (fromIntegral n + 1)
                                      , currCount = 0
                                      , currEMA = 0 }

append :: (Fractional a) => a -> ExpMovingAvg a -> ExpMovingAvg a
append a m =
    if currCount m < count m
     then m { currCount = currCount m + 1
            , currEMA = currEMA m + a / (fromIntegral $ count m) }
     else m { currEMA = currEMA m * (1 - k m) + a * k m }

lookupEMA :: ExpMovingAvg a -> Maybe a
lookupEMA m = do
    guard $ currCount m == count m
    Just $ currEMA m

--------------------------------------------------------------------------------
