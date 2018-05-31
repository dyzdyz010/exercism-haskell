module RunLength (decode, encode) where

import Data.Char(digitToInt, isNumber)
import Data.List(concat, group)

decode :: String -> String
decode xs = decode' xs ' ' 1

decode' :: String -> Char -> Int -> String
decode' [] _ _ = []
decode' (x: xs) l c = case isNumber x of
  True -> decode' xs x (digitToInt x + (if isNumber l then (10*) $ digitToInt l else 0))
  False -> (replicate c x) ++ decode' xs x 1

encode :: String -> String
encode text = concat $ map (\x -> (if length x == 1 then "" else show $ length x)  ++ [x !! 0]) $ group text
