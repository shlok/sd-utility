--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

--------------------------------------------------------------------------------

module SD.Utility.Exception where

--------------------------------------------------------------------------------

import Control.Exception (Exception, SomeException, fromException)

--------------------------------------------------------------------------------

-- | 'f `holdsFor` se' returns 'True' iff 'se' has the
-- given exception type and 'f' called on it returns 'True'.
holdsFor :: forall e . Exception e => (e -> Bool) -> SomeException -> Bool
holdsFor f se =
    case (fromException @e) se of
        Nothing -> False
        Just e -> f e

--------------------------------------------------------------------------------
