module Atbash (decode, encode) where

import           Data.Char (isLetter, isNumber, toLower)
import           Data.List (elemIndex, intercalate, splitAt)

decode :: String -> String
decode [] = []
decode (c: xs) = (if isNumber c then [c] else if isLetter c then
  [let Just ind = elemIndex c ['z', 'y'..'a'] in ['a'..'z'] !! ind] else "")
  ++ (decode xs)

encode :: String -> String
encode xs = intercalate " " $ splitEvery 5 $ encode' xs

splitEvery :: Int -> String -> [String]
splitEvery _ [] = []
splitEvery n xs = as : (splitEvery n bs)
  where (as, bs) = splitAt n xs

encode' :: String ->String
encode' [] = []
encode' (c: xs) = (if isNumber c then
                         [c]
                       else if isLetter c then
                         [let Just ind = elemIndex (toLower c) ['a'..'z'] in ['z', 'y'..'a'] !! ind]
                       else "") ++ (encode' xs)