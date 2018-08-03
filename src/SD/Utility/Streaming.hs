--------------------------------------------------------------------------------

{-# LANGUAGE BangPatterns #-}

--------------------------------------------------------------------------------

module SD.Utility.Streaming (
    streamLines
  , mapWhile
  , takeWhile'
  , slidingWindow'
  ) where

--------------------------------------------------------------------------------

import Control.Monad.Except (MonadError, throwError)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Streaming.Char8 as SB (ByteString, splitWith, toStrict)
import Data.Sequence (Seq((:<|), Empty))
import qualified Data.Sequence as Seq (length)
import Streaming.Prelude (Of ((:>)), Stream, copy, mapped, slidingWindow, yield)
import qualified Streaming.Prelude as S (filter, last, map, takeWhileM)

--------------------------------------------------------------------------------

streamLines :: (Monad m) => SB.ByteString m r -> Stream (Of ByteString) m r
streamLines = (mapped SB.toStrict) . SB.splitWith (=='\n')

--------------------------------------------------------------------------------

-- | Maps over a stream, ending it when a mapped element fails a condition.
-- Throws a projection of the corresponding original (unmapped) element.
-- The original return value is lost.
--
-- An example of how throwing (a projection of) the /original/ element can be useful:
-- We are mapping into Maybe values and want to end on Nothing; throwing (a projection)
-- of the original element is more informative.
mapWhile :: (MonadError e m) => (a -> b) -> (b -> Bool) -> (a -> e) -> Stream (Of a) m r -> Stream (Of b) m ()
mapWhile mp p ae stream = S.map snd . S.takeWhileM predM $ S.map (\a -> (a, mp a)) stream
    where predM (a, b) = if p b
                          then return True
                          else throwError $ ae a

-- | Ends a stream when an element fails a condition. Throws a projection
-- of the failed element. The original return value is lost.
takeWhile' :: (MonadError e m) => (a -> Bool) -> (a -> e) -> Stream (Of a) m r -> Stream (Of a) m ()
takeWhile' = mapWhile id

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
