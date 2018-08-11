module SecretHandshake (handshake) where

import Numeric(showIntAtBase)
import Data.List(mapAccumL)
import Data.Char(intToDigit)

handshake :: Int -> [String]
handshake n = (if length digits > 4 && digits !! 4 == '1' then reverse else id) $ filter (\x -> x /= "") $ snd $ mapAccumL handshake' 0 digits
  where digits = reverse $ showIntAtBase 2 intToDigit n ""

handshake' :: Int -> Char -> (Int, String)
handshake' n digit = if digit == '1' then (n + 1, (case n of
    0 -> "wink"
    1 -> "double blink"
    2 -> "close your eyes"
    3 -> "jump"
    otherwise -> ""))
  else
    (n+1, "")