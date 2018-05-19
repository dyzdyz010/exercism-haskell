module Squares (difference, squareOfSums, sumOfSquares) where

difference :: Integral a => a -> a
difference n = squareOfSums n - sumOfSquares n

squareOfSums :: Integral a => a -> a
squareOfSums n = sum [1..n] ^ 2

sumOfSquares :: Integral a => a -> a
sumOfSquares 1 = 1
sumOfSquares n = n^2 + sumOfSquares (n - 1)
