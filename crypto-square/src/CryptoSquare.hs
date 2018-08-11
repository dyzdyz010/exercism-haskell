module CryptoSquare (encode) where

import Data.Char(isAlphaNum, toLower)
import Data.List(intercalate)

encode :: String -> String
encode "" = ""
encode xs = intercalate " " $ encode' (completion chars (r, c)) (r, c) 0
  where chars = [toLower x | x <- xs, isAlphaNum x]
        (r, c) = head [(r, c) |
                       r <- [1..10],
                       c <- [1..10],
                       c >= r, c - r <= 1,
                       c*r >= length chars,
                       (c * r) - length chars < c]

encode' :: String -> (Int, Int) -> Int -> [String]
encode' _ (r, _) offset | r < offset = []
encode' str (r, c) offset = [x |
                     x <- [s | (s, j) <- zip str [0..length str - 1], j `mod` c == offset]
                    ] : (encode' str (r, c) $ offset + 1)

completion :: String -> (Int, Int) -> String
completion str (r, c) = str ++ replicate (r*c - length str) ' '
