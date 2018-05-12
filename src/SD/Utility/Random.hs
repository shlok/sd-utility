--------------------------------------------------------------------------------

module SD.Utility.Random (
    randomSplitList
  , randomSplitSeq
) where

--------------------------------------------------------------------------------

import Data.Foldable (toList)
import Data.Sequence (Seq (Empty, (:<|), (:|>)), empty, fromList, singleton)
import System.Random (RandomGen, newStdGen, random)

--------------------------------------------------------------------------------

randomSplitList :: [a] -> IO [[a]]
randomSplitList as = (map toList . toList) <$> (randomSplitSeq $ fromList as)

randomSplitSeq :: Seq a -> IO (Seq (Seq a))
randomSplitSeq as = do
    g <- newStdGen
    let (ss, _) = randomSplitGo g as empty
    return ss

-- Idea: The next element being processed has a 50/50 chance of either
-- ending up in the last existing sequence or a new singleton sequence.
randomSplitGo :: (RandomGen g) => g -> Seq a -> Seq (Seq a) -> (Seq (Seq a), g)
randomSplitGo g Empty sss = (sss, g)
randomSplitGo g (a :<| as) sss =
    let (outcomeOne, g') = random g -- Random boolean.
        sss' = if outcomeOne
                then appendToLast a sss
                else appendAsNewSingletonSeq a sss
     in randomSplitGo g' as sss'

-- Outcome 1.
-- * If the provided sequence is not empty, append the provided
--   element to the last sequence in the provided sequence.
-- * If the provided sequence is empty, return a new singleton
--   sequence containing the provided element wrapped in a singleton.
appendToLast :: a -> Seq (Seq a) -> Seq (Seq a)
appendToLast a (ss :|> s) = ss :|> (s :|> a)
appendToLast a Empty = singleton $ singleton a

-- Outcome 2.
-- Append the provided element wrapped in a singleton to the provided sequence.
appendAsNewSingletonSeq :: a -> Seq (Seq a) -> Seq (Seq a)
appendAsNewSingletonSeq a ss = ss :|> singleton a

--------------------------------------------------------------------------------
