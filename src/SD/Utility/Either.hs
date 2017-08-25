--------------------------------------------------------------------------------

module SD.Utility.Either where

--------------------------------------------------------------------------------

leftToRight :: (a -> Bool) -> b -> Either a b -> Either a b
leftToRight f b e =
    case e of
        Left a -> if f a then Right b else Left a
        Right b' -> Right b'

rightToLeft :: (b -> Bool) -> a -> Either a b -> Either a b
rightToLeft f a e =
    case e of
        Left a' -> Left a'
        Right b -> if f b then Left a else Right b

--------------------------------------------------------------------------------
