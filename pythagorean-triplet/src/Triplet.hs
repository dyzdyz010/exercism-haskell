module Triplet (isPythagorean, mkTriplet, pythagoreanTriplets) where

import           Data.List (sort)

isPythagorean :: (Int, Int, Int) -> Bool
isPythagorean (x, y, z) = x^2 + y^2 == z^2

mkTriplet :: Int -> Int -> Int -> (Int, Int, Int)
mkTriplet a b c = (x, y, z)
  where [x, y, z] = sort [a, b, c]

pythagoreanTriplets :: Int -> Int -> [(Int, Int, Int)]
pythagoreanTriplets minFactor maxFactor =
  [(x, y, z)
  | (x, i) <- pairs,
    (y, j) <- pairs,
    (z, k) <- pairs,
    x^2 + y^2 == z^2,
    k > j && j > i]
  where pairs = zip [minFactor..maxFactor] [1..]
