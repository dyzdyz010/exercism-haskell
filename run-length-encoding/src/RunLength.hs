module RunLength (decode, encode) where

import Data.Char(digitToInt, isNumber)

decode :: String -> String
decode xs = decode' xs ' ' 1

decode' :: String -> Char -> Int -> String
decode' [] _ _ = []
decode' (x: xs) l c = case isNumber x of
  True -> decode' xs x (digitToInt x + (if isNumber l then (10*) $ digitToInt l else 0))
  False -> (replicate c x) ++ decode' xs x 1

encode :: String -> String
encode text = encode' text ' ' 0

encode' :: String -> Char -> Int -> String
encode' [] _ 0 = []
encode' [] l c = (if c /= 1 then show c else "") ++ [l]
encode' (x: xs) l c = case x == l of
  True -> encode' xs x $ c + 1
  False -> (if c /= 0 then (if c /= 1 then show c else "") ++ [l] else "") ++ encode' xs x 1
