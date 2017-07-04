{-# LANGUAGE BangPatterns #-}

module SD.Utility.Scientific
    ( scientificP
    ) where

import           Data.Scientific                 (Scientific, scientific)
import           Control.Monad                   (mplus)
import           Data.Char                       (ord)
import qualified Text.ParserCombinators.ReadP    as ReadP
import           Text.ParserCombinators.ReadP    (ReadP)

-- The below was mostly copied from Data.Scientific in scientific-0.3.4.15.
-- Copyright: Bas van Dijk 2013. License: BSD3.
-- https://hackage.haskell.org/package/scientific-0.3.4.15/docs/src/Data-Scientific.html
-- (The only difference is that at the end of the scientificP function,
-- we use the scientific function to create a Scientific.)
--
-- Purpose: Publicly expose scientificP so we can parse
-- floating-point numbers in our own parser combinators.

-- A strict pair
data SP = SP !Integer {-# UNPACK #-}!Int

scientificP :: ReadP Scientific
scientificP = do
  let positive = (('+' ==) <$> ReadP.satisfy isSign) `mplus` return True
  pos <- positive

  let step :: Num a => a -> Int -> a
      step a digit = a * 10 + fromIntegral digit
      {-# INLINE step #-}

  n <- foldDigits step 0

  let s = SP n 0
      fractional = foldDigits (\(SP a e) digit ->
                                 SP (step a digit) (e-1)) s

  SP coeff expnt <- (ReadP.satisfy (== '.') >> fractional)
                    ReadP.<++ return s

  let signedCoeff | pos       =   coeff
                  | otherwise = (-coeff)

      eP = do posE <- positive
              e <- foldDigits step 0
              if posE
                then return   e
                else return (-e)

  (ReadP.satisfy isE >>
           ((scientific signedCoeff . (expnt +)) <$> eP)) `mplus`
     return (scientific signedCoeff    expnt)


foldDigits :: (a -> Int -> a) -> a -> ReadP a
foldDigits f z = do
    c <- ReadP.satisfy isDecimal
    let digit = ord c - 48
        a = f z digit

    ReadP.look >>= go a
  where
    go !a [] = return a
    go !a (c:cs)
        | isDecimal c = do
            _ <- ReadP.get
            let digit = ord c - 48
            go (f a digit) cs
        | otherwise = return a

isDecimal :: Char -> Bool
isDecimal c = c >= '0' && c <= '9'
{-# INLINE isDecimal #-}

isSign :: Char -> Bool
isSign c = c == '-' || c == '+'
{-# INLINE isSign #-}

isE :: Char -> Bool
isE c = c == 'e' || c == 'E'
{-# INLINE isE #-}
