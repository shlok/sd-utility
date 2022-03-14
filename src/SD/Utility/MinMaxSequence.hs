-- | The @'MMSeq' a@ type represents a sequence with a given maximum
-- length. Appending to an already full 'MMSeq' results in an element
-- first getting removed from the front. 'MMSeq' has support for fast
-- querying of the minimum or maximum value.
module SD.Utility.MinMaxSequence
  ( MMSeq (),
    MMSeqSetting (..),
    empty,
    append,
    lookupValue,
    lookupFullValue,
  )
where

import Control.Monad (guard)
import Data.Sequence (Seq (Empty, (:<|)), (|>))
import qualified Data.Sequence as Seq
import SD.Utility.MaxLengthSequence (MLSeq, full)
import qualified SD.Utility.MaxLengthSequence as MLSeq (append, empty)

data MMSeqSetting = MMSeqMin | MMSeqMax

data MMSeq a = MMSeq
  { -- The element index is required for correct removal from the front;
    -- see https://www.geeksforgeeks.org/sliding-window-maximum-maximum-of-all-subarrays-of-size-k/
    mlSeq :: !(MLSeq (a, Int)),
    mlSeqLen :: !Int,
    setting :: !MMSeqSetting,
    lastIndex :: !Int,
    sequ :: !(Seq (a, Int))
  }

-- | Please provide a positive maximum length. (Returns 'Nothing' otherwise.)
empty :: Int -> MMSeqSetting -> Maybe (MMSeq a)
empty n sett = do
  mls <- MLSeq.empty n
  return $
    MMSeq
      { mlSeq = mls,
        mlSeqLen = n,
        setting = sett,
        lastIndex = -1,
        sequ = Seq.empty
      }

append :: (Ord a) => a -> MMSeq a -> MMSeq a
append
  a
  s@MMSeq
    { mlSeq = currSeq,
      mlSeqLen = mlSeqLen', -- Constant.
      setting = sett, -- Constant.
      lastIndex = lastIdx,
      sequ = currSequ
    } =
    let newIdx = lastIdx + 1
        newSeq = MLSeq.append (a, newIdx) currSeq
     in if full currSeq
          then
            s
              { mlSeq = newSeq,
                lastIndex = newIdx,
                sequ = cleanBack sett a (cleanFront newIdx mlSeqLen' currSequ) |> (a, newIdx)
              }
          else
            s
              { mlSeq = newSeq,
                lastIndex = newIdx,
                sequ = cleanBack sett a currSequ |> (a, newIdx)
              }

{-# INLINE cleanFront #-}
cleanFront :: Int -> Int -> Seq (a, Int) -> Seq (a, Int)
cleanFront newIdx mlSeqLen' = Seq.dropWhileL (\(_, idx) -> idx <= newIdx - mlSeqLen')

{-# INLINE cleanBack #-}
cleanBack :: (Ord a) => MMSeqSetting -> a -> Seq (a, Int) -> Seq (a, Int)
cleanBack set a =
  Seq.dropWhileR
    ( \(a', _) ->
        case set of
          MMSeqMin -> a' >= a
          MMSeqMax -> a' <= a
    )

{-# INLINE lookupValue #-}
lookupValue :: MMSeq a -> Maybe a
lookupValue MMSeq {sequ = Empty} = Nothing
lookupValue MMSeq {sequ = head' :<| _} = Just $ fst head'

{-# INLINE lookupFullValue #-}
lookupFullValue :: MMSeq a -> Maybe a
lookupFullValue s = do
  guard . full $ mlSeq s
  lookupValue s
