--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

--------------------------------------------------------------------------------

module SD.Utility.Read where

--------------------------------------------------------------------------------

import Data.Char (isDigit)
import Text.Read (readMaybe)
import Text.ParserCombinators.ReadP (ReadP, many1, pfail, satisfy)

--------------------------------------------------------------------------------

integralP :: forall a . (Integral a, Read a) => ReadP a
integralP = do
    digits <- many1 $ satisfy isDigit
    let mIntegral = readMaybe @a digits
    case mIntegral of
        Nothing -> pfail
        Just i -> return i

--------------------------------------------------------------------------------
