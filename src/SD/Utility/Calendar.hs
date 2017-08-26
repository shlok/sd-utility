--------------------------------------------------------------------------------

module SD.Utility.Calendar where

--------------------------------------------------------------------------------

import Data.Time.Calendar (Day)
import Data.Time.Calendar.Easter (sundayAfter)

--------------------------------------------------------------------------------

sundayBetween :: Day -> Day -> Bool
sundayBetween d1 d2
    | d1 == d2 = False
    | d1 > d2 = sundayBetween d2 d1
    | otherwise = let sunday = sundayAfter d1
                   in d1 < sunday && sunday < d2

--------------------------------------------------------------------------------
