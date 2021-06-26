--------------------------------------------------------------------------------

module SD.Utility.ExpMovingAverage (
      EMA ()
    , empty
    , append
    , lookupEMA
    ) where

--------------------------------------------------------------------------------

import Control.Monad (guard)

--------------------------------------------------------------------------------

data EMA a = EMA {
      count     :: !Int
    , k         :: !a
    , currCount :: !Int
    , currEMA   :: !a
    }

-- | Please provide a positive count. (Returns 'Nothing' otherwise.)
empty :: (Fractional a) => Int -> Maybe (EMA a)
empty n
    | n <= 0 = Nothing
    | otherwise = Just $ EMA { count = n
                             , k = 2 / (fromIntegral n + 1)
                             , currCount = 0
                             , currEMA = 0 }

append :: (Fractional a) => a -> EMA a -> EMA a
append a m =
    if currCount m < count m
     then m { currCount = currCount m + 1
            , currEMA = currEMA m + a / fromIntegral (count m) }
     else m { currEMA = currEMA m * (1 - k m) + a * k m }

lookupEMA :: EMA a -> Maybe a
lookupEMA m = do
    guard $ currCount m == count m
    Just $ currEMA m

--------------------------------------------------------------------------------
