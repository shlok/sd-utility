--------------------------------------------------------------------------------

-- | The @'MMSeq' a@ type represents a sequence with a given maximum
-- length. Appending to an already full 'MMSeq' results in an element
-- first getting removed from the front. 'MMSeq' has support for fast
-- querying of the minimum and maximum values.

--------------------------------------------------------------------------------

module SD.Utility.MinMaxSequence (
      MMSeq ()
    , empty
    , append
    , lookupMin
    , lookupMax
    , lookupFullMin
    , lookupFullMax
    ) where

--------------------------------------------------------------------------------

import SD.Utility.MaxLengthSequence (MLSeq, full)
import qualified SD.Utility.MaxLengthSequence as MLSeq (append, empty, head)

import Control.Monad (guard)
import Data.Maybe (fromJust)
import Data.Map.Strict (Map, insertWith, update)
import qualified Data.Map.Strict as Map (empty, lookupMax, lookupMin)

--------------------------------------------------------------------------------

data MMSeq a = MMSeq {
      maxLength :: !Int
    , mlSeq     :: !(MLSeq a)
    , elemsMap  :: !(Map a Int)
    }

-- | Please provide a positive maximum length. (Returns 'Nothing' otherwise.)
empty :: Int -> Maybe (MMSeq a)
empty n = do
    mls <- MLSeq.empty n
    return $ MMSeq { maxLength = n
                   , mlSeq = mls
                   , elemsMap = Map.empty }

append :: (Ord a) => a -> MMSeq a -> MMSeq a
append a s =
    let currSeq = mlSeq s
        currMap = elemsMap s
        newSeq = MLSeq.append a currSeq
     in if full currSeq
         then s { mlSeq = newSeq
                , elemsMap = insert' a $ delete' (fromJust $ MLSeq.head currSeq) currMap }
         else s { mlSeq = newSeq
                , elemsMap = insert' a currMap }

lookupMin :: MMSeq a -> Maybe a
lookupMin s = fst <$> Map.lookupMin (elemsMap s)

lookupMax :: MMSeq a -> Maybe a
lookupMax s = fst <$> Map.lookupMax (elemsMap s)

lookupFullMin :: MMSeq a -> Maybe a
lookupFullMin s = do
    guard $ full $ mlSeq s
    (k, _) <- Map.lookupMin $ elemsMap s
    return k

lookupFullMax :: MMSeq a -> Maybe a
lookupFullMax s = do
    guard $ full $ mlSeq s
    (k, _) <- Map.lookupMax $ elemsMap s
    return k

--------------------------------------------------------------------------------

insert' :: (Ord k) => k -> Map k Int -> Map k Int
insert' k = insertWith (+) k 1

delete' :: (Ord k) => k -> Map k Int -> Map k Int
delete' = update $ \i -> if i == 1
                          then Nothing
                          else Just $ i - 1

--------------------------------------------------------------------------------
