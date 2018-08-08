--------------------------------------------------------------------------------

module SD.Utility.AverageTrueRange (
      ATR ()
    , empty
    , append
    , lookupATR
    ) where

--------------------------------------------------------------------------------

import SD.Utility.ExpMovingAverage (EMA, lookupEMA)
import qualified SD.Utility.ExpMovingAverage as EMA (append, empty)

--------------------------------------------------------------------------------

data ATR a = ATR {
      ema       :: !(EMA a)
    , lastClose :: !(Maybe a)
    }

-- | Please provide a positive count. (Returns 'Nothing' otherwise.)
empty :: (Fractional a) => Int -> Maybe (ATR a)
empty n = do
    ema' <- EMA.empty n
    Just $ ATR { ema = ema'
               , lastClose = Nothing }

-- | Append (high, low, close) to an ATR.
append :: (Fractional a, Ord a) => (a, a, a) -> ATR a -> ATR a
append (h, l, c) atr =
    let (currEma, mLc) = (ema atr, lastClose atr)
     in case mLc of
            Nothing -> ATR { ema = currEma, lastClose = Just c }
            Just lc -> ATR { ema = let tr = maximum [h - l, abs $ lc - h, abs $ lc - l]
                                    in EMA.append tr currEma
                           , lastClose = Just c }

lookupATR :: ATR a -> Maybe a
lookupATR = lookupEMA . ema

--------------------------------------------------------------------------------
