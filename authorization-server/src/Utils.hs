module Utils where

import qualified Data.Text.Lazy as LT

lazyTextToString :: LT.Text -> String
lazyTextToString = LT.unpack

stringToInt :: String -> Int
stringToInt = read