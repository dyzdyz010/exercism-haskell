module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys | length xs /= length ys = Nothing
distance xs ys = Just $ length [(x, y) | (x, y) <- zip xs ys, x /= y]