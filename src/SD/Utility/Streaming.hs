--------------------------------------------------------------------------------

module SD.Utility.Streaming where

--------------------------------------------------------------------------------

import Data.ByteString (ByteString)
import qualified Data.ByteString.Streaming.Char8 as SB (ByteString, splitWith, toStrict)
import Streaming.Prelude (Of, Stream, mapped)

--------------------------------------------------------------------------------

streamLines :: (Monad m) => SB.ByteString m r -> Stream (Of ByteString) m r
streamLines = (mapped SB.toStrict) . SB.splitWith (=='\n')

--------------------------------------------------------------------------------
