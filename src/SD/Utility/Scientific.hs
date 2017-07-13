--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

--------------------------------------------------------------------------------

module SD.Utility.Scientific where

--------------------------------------------------------------------------------

import Data.Scientific (Scientific, toBoundedRealFloat, scientificP)
import Text.ParserCombinators.ReadP (ReadP, pfail)

--------------------------------------------------------------------------------

realFloatP :: forall a . RealFloat a => ReadP a
realFloatP = do
    scientific <- scientificP
    let mRealFloat = scientificToRealFloat @a scientific
    case mRealFloat of
        Nothing -> pfail
        Just realFloat -> return realFloat

scientificToRealFloat :: forall a . RealFloat a => Scientific -> Maybe a
scientificToRealFloat s =
    case toBoundedRealFloat @a s of
        Left _ -> Nothing
        Right rf -> Just rf

--------------------------------------------------------------------------------
