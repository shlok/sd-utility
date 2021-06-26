--------------------------------------------------------------------------------

-- | The @'MLSeq' a@ type represents a sequence with a given maximum length.
-- Appending to an already full 'MLSeq' results in an element first getting
-- removed from the front.

--------------------------------------------------------------------------------

module SD.Utility.MaxLengthSequence (
    MLSeq ()
  , empty
  , head
  , length
  , full
  , append
  , sequ
  ) where

--------------------------------------------------------------------------------

import Prelude hiding (head, length)
import Data.Sequence (Seq((:<|), Empty), (|>), singleton)
import qualified Data.Sequence as Seq (empty, length)

--------------------------------------------------------------------------------

data MLSeq a = MLSeq {
      maxLength :: !Int
    , elemsSeq  :: !(Seq a)
    }
    deriving (Show)

-- | Please provide a positive maximum length. (Returns 'Nothing' otherwise.)
empty :: Int -> Maybe (MLSeq a)
empty n
    | n <= 0 = Nothing
    | otherwise = Just $ MLSeq { maxLength = n
                               , elemsSeq = Seq.empty }
{-# INLINE empty #-}

head :: MLSeq a -> Maybe a
head MLSeq { elemsSeq = Empty } = Nothing
head MLSeq { elemsSeq = head' :<| _ } = Just head'
{-# INLINE head #-}

length :: MLSeq a -> Int
length = Seq.length . elemsSeq
{-# INLINE length #-}

full :: MLSeq a -> Bool
full s = maxLength s == length s
{-# INLINE full #-}

append :: a -> MLSeq a -> MLSeq a
append a s @ MLSeq { elemsSeq = Empty } = s { elemsSeq = singleton a }
append a s @ MLSeq { elemsSeq = currSeq @ (_ :<| currSeqTail) } =
    if full s
     then s { elemsSeq = currSeqTail |> a }
     else s { elemsSeq = currSeq |> a }
{-# INLINE append #-}

sequ :: MLSeq a -> Seq a
sequ = elemsSeq
{-# INLINE sequ #-}

--------------------------------------------------------------------------------
