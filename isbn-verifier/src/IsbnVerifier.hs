module IsbnVerifier (isbn) where

import Data.Char(isNumber, digitToInt)
import Data.List(elemIndex)

isbn :: String -> Bool
isbn xs | (length $ digits xs) /= 10 = False
isbn xs = (calculate (digits xs) 10) `mod` 11 == 0

digits :: String -> String
digits xs = filter (\x -> isNumber x || (x == 'X' && let Just xi = elemIndex 'X' xs in xi  == length xs - 1)) xs

calculate :: String -> Int -> Int
calculate _ 0 = 0
calculate (x: xs) d = d * (if x == 'X' then 10 else digitToInt x) + calculate xs (d - 1)