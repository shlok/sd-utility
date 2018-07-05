--------------------------------------------------------------------------------

module SD.Utility.Streaming where

--------------------------------------------------------------------------------

import Control.Monad.Except (MonadError, throwError)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Streaming.Char8 as SB (ByteString, splitWith, toStrict)
import Streaming.Prelude (Of, Stream, mapped)
import qualified Streaming.Prelude as S (map, takeWhileM)

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
