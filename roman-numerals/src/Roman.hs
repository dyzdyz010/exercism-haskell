module Roman (numerals) where

import Data.List(genericReplicate)

numerals :: Integer -> Maybe String
numerals n | n < 0 = Nothing
numerals n = Just $ numerals' n 3

numerals' :: Integer -> Int -> String
numerals' _ u | u < 0 = ""
numerals' n u = digit (n `div` 10 ^ u) u ++ numerals' (n `mod` 10 ^ u) (u-1)

units :: String
units = "IVXLCDM"

digit :: Integer -> Int -> String
digit d unit = case d of
  0 -> ""
  x | x `elem` [1..3] -> genericReplicate x one
  4 -> [one, five]
  5 -> [five]
  x | x `elem` [6..8] -> five : genericReplicate (x - 5) one
  9 ->  [one, ten]
  where one = units !! (unit * 2)
        five = units !! (unit * 2 + 1)
        ten = units !! (unit * 2 + 2)
