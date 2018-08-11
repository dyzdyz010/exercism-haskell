module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples xs limit =
  sum [a | a <- [1..limit], a < limit, any (\b -> a `mod` b == 0) xs]