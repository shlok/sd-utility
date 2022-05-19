{-# LANGUAGE NamedFieldPuns #-}

-- | This module is intended to be imported qualified: @import qualified
-- SD.Utility.RelativeStrengthIndex as RSI@.
module SD.Utility.RelativeStrengthIndex
  ( RSI (),
    empty,
    append,
    lookup,
  )
where

import SD.Utility.ExpMovingAverage (EMA)
import qualified SD.Utility.ExpMovingAverage as EMA
import Prelude hiding (lookup)

data RSI a = RSI
  { smmaProfits :: !(EMA a),
    smmaLosses :: !(EMA a),
    lasta :: !(Maybe a)
  }

-- | Please provide a positive count. (Returns 'Nothing' otherwise.)
empty :: (Fractional a) => Int -> Maybe (RSI a)
empty n = do
  smmaProfits' <- EMA.emptySmoothed n
  smmaLosses' <- EMA.emptySmoothed n
  Just $
    RSI
      { smmaProfits = smmaProfits',
        smmaLosses = smmaLosses',
        lasta = Nothing
      }

append :: (Fractional a, Ord a) => a -> RSI a -> RSI a
append a rsi@RSI {smmaProfits, smmaLosses, lasta = mlasta} =
  case mlasta of
    Nothing -> rsi {lasta = Just a}
    Just lasta ->
      let profit = max (a - lasta) 0
          loss = max (negate $ a - lasta) 0
       in RSI
            { smmaProfits = EMA.append profit smmaProfits,
              smmaLosses = EMA.append loss smmaLosses,
              lasta = Just a
            }

lookup :: (Eq a, Fractional a) => RSI a -> Maybe a
lookup RSI {smmaProfits, smmaLosses} = do
  smmaProfits' <- EMA.lookup smmaProfits
  smmaLosses' <- EMA.lookup smmaLosses
  if smmaLosses' == 0
    then Just 100
    else Just $ 100 - 100 / (1 + smmaProfits' / smmaLosses')
