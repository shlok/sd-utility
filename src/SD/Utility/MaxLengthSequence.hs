-- | The @'MLSeq' a@ type represents a sequence with a given maximum length. Appending to an already
-- full 'MLSeq' results in an element first getting removed from the front.
--
-- This module is intended to be imported qualified: @import qualified SD.Utility.MaxLengthSequence
-- as MLSeq@.
module SD.Utility.MaxLengthSequence
  ( MLSeq (),
    empty,
    head,
    length,
    full,
    append,
    sequ,
  )
where

import Data.Sequence (Seq (Empty, (:<|)), singleton, (|>))
import qualified Data.Sequence as Seq (empty, length)
import Prelude hiding (head, length)

data MLSeq a = MLSeq
  { maxLength :: !Int,
    elemsSeq :: !(Seq a)
  }
  deriving (Show)

-- | Please provide a positive maximum length. (Returns 'Nothing' otherwise.)
{-# INLINE empty #-}
empty :: Int -> Maybe (MLSeq a)
empty n
  | n <= 0 = Nothing
  | otherwise =
    Just $
      MLSeq
        { maxLength = n,
          elemsSeq = Seq.empty
        }

{-# INLINE head #-}
head :: MLSeq a -> Maybe a
head MLSeq {elemsSeq = Empty} = Nothing
head MLSeq {elemsSeq = head' :<| _} = Just head'

{-# INLINE length #-}
length :: MLSeq a -> Int
length = Seq.length . elemsSeq

{-# INLINE full #-}
full :: MLSeq a -> Bool
full s = maxLength s == length s

{-# INLINE append #-}
append :: a -> MLSeq a -> MLSeq a
append a s@MLSeq {elemsSeq = Empty} = s {elemsSeq = singleton a}
append a s@MLSeq {elemsSeq = currSeq@(_ :<| currSeqTail)} =
  if full s
    then s {elemsSeq = currSeqTail |> a}
    else s {elemsSeq = currSeq |> a}

{-# INLINE sequ #-}
sequ :: MLSeq a -> Seq a
sequ = elemsSeq
