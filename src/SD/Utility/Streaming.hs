--------------------------------------------------------------------------------

{-# LANGUAGE BangPatterns #-}

--------------------------------------------------------------------------------

module SD.Utility.Streaming (
    slidingWindow'
  ) where

--------------------------------------------------------------------------------

import Data.Sequence (Seq((:<|), Empty))
import qualified Data.Sequence as Seq (length)
import Streaming.Prelude (Of ((:>)), Stream, copy, slidingWindow, yield)
import qualified Streaming.Prelude as S (filter, last)

--------------------------------------------------------------------------------

-- | Just like the built-in @slidingWindow@; except that (1) sequences shorter than
-- the window at the end of the stream are included, and (2) if the original stream
-- is empty, the resulting stream is empty (as opposed to having a single empty sequence).
--
-- > > import qualified Streaming.Prelude as S
-- > > S.print $ slidingWindow' 3 $ S.each ("1234" :: String)
-- > fromList "123"
-- > fromList "234"
-- > fromList "34"
-- > fromList "4"
slidingWindow' :: (Monad m) => Int -> Stream (Of a) m r -> Stream (Of (Seq a)) m r
slidingWindow' n str = do
    mLastSequ :> r <- S.last . copy . S.filter (\sequ -> Seq.length sequ /= 0) $ slidingWindow n str
    case mLastSequ of
        Nothing -> return r
        Just lastSequ -> yieldRem r lastSequ
{-# INLINABLE slidingWindow' #-}

yieldRem :: (Monad m) => r -> Seq a -> Stream (Of (Seq a)) m r
yieldRem r Empty = return r
yieldRem r (_ :<| Empty) = return r
yieldRem r (_ :<| end) = yield end >> yieldRem r end
{-# INLINABLE yieldRem #-}

--------------------------------------------------------------------------------
