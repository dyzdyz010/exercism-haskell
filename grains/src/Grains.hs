module Grains (square, total) where

square :: Integer -> Maybe Integer
square n
  | n `elem` [1..64] = Just $ 2 ^ (n - 1)
  | otherwise = Nothing

total :: Integer
total = sum [x | (Just x) <- [square a | a <- [1..64]]]