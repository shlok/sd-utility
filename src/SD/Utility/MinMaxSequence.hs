-- | The @'MMSeq' a@ type represents a sequence with a given maximum
-- length. Appending to an already full 'MMSeq' results in an element
-- first getting removed from the front. 'MMSeq' has support for fast
-- querying of the minimum and maximum values.
module SD.Utility.MinMaxSequence
  ( MMSeq (),
    empty,
    append,
    lookupMin,
    lookupMax,
    lookupFullMin,
    lookupFullMax,
  )
where

import Control.Monad (guard)
import Data.Sequence (Seq (Empty, (:<|)), (|>))
import qualified Data.Sequence as Seq
import SD.Utility.MaxLengthSequence (MLSeq, full)
import qualified SD.Utility.MaxLengthSequence as MLSeq (append, empty)

data MMSeq a = MMSeq
  { -- The element index is required for correct removal from the front;
    -- see https://www.geeksforgeeks.org/sliding-window-maximum-maximum-of-all-subarrays-of-size-k/
    mlSeq :: !(MLSeq (a, Int)),
    mlSeqLen :: !Int,
    lastIndex :: !Int,
    maxSeq :: !(Seq (a, Int)),
    minSeq :: !(Seq (a, Int))
  }

-- | Please provide a positive maximum length. (Returns 'Nothing' otherwise.)
empty :: Int -> Maybe (MMSeq a)
empty n = do
  mls <- MLSeq.empty n
  return $
    MMSeq
      { mlSeq = mls,
        mlSeqLen = n,
        lastIndex = -1,
        maxSeq = Seq.empty,
        minSeq = Seq.empty
      }

append :: (Ord a) => a -> MMSeq a -> MMSeq a
append
  a
  s@MMSeq
    { mlSeq = currSeq,
      mlSeqLen = mlSeqLen', -- Constant.
      lastIndex = lastIdx,
      maxSeq = currMaxSeq,
      minSeq = currMinSeq
    } =
    let newIdx = lastIdx + 1
        newSeq = MLSeq.append (a, newIdx) currSeq
     in if full currSeq
          then
            s
              { mlSeq = newSeq,
                lastIndex = newIdx,
                maxSeq = cleanBack True a (cleanFront newIdx mlSeqLen' currMaxSeq) |> (a, newIdx),
                minSeq = cleanBack False a (cleanFront newIdx mlSeqLen' currMinSeq) |> (a, newIdx)
              }
          else
            s
              { mlSeq = newSeq,
                lastIndex = newIdx,
                maxSeq = cleanBack True a currMaxSeq |> (a, newIdx),
                minSeq = cleanBack False a currMinSeq |> (a, newIdx)
              }

{-# INLINE cleanFront #-}
cleanFront :: Int -> Int -> Seq (a, Int) -> Seq (a, Int)
cleanFront newIdx mlSeqLen' = Seq.dropWhileL (\(_, idx) -> idx <= newIdx - mlSeqLen')

{-# INLINE cleanBack #-}
cleanBack :: (Ord a) => Bool -> a -> Seq (a, Int) -> Seq (a, Int)
cleanBack useMax a = Seq.dropWhileR (\(a', _) -> if useMax then a' <= a else a' >= a)

lookupMin :: MMSeq a -> Maybe a
lookupMin MMSeq {minSeq = Empty} = Nothing
lookupMin MMSeq {minSeq = head' :<| _} = Just $ fst head'

lookupMax :: MMSeq a -> Maybe a
lookupMax MMSeq {maxSeq = Empty} = Nothing
lookupMax MMSeq {maxSeq = head' :<| _} = Just $ fst head'

lookupFullMin :: MMSeq a -> Maybe a
lookupFullMin s = do
  guard . full $ mlSeq s
  lookupMin s

lookupFullMax :: MMSeq a -> Maybe a
lookupFullMax s = do
  guard . full $ mlSeq s
  lookupMax s
