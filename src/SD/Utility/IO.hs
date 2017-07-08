{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------

module SD.Utility.IO where

--------------------------------------------------------------------------------

import Control.Exception (SomeException, catch)
import System.IO         (FilePath, Handle, IOMode)
import System.IO         as S (hClose, openFile)

--------------------------------------------------------------------------------

openFile :: FilePath -> IOMode -> IO (Either SomeException Handle)
openFile p m = (Right <$> S.openFile p m) `catch`
    (\(e :: SomeException) -> return . Left $ e)

--------------------------------------------------------------------------------

hClose :: Handle -> IO (Either SomeException ())
hClose h = (Right <$> S.hClose h) `catch`
    (\(e :: SomeException) -> return . Left $ e)
