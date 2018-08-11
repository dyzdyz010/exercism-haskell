module Triangle (TriangleType(..), triangleType) where

import Data.List

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Ord a, Num a) => a -> a -> a -> TriangleType
triangleType a b c
  | x + y <= z = Illegal
  | (length $ nub [a, b, c]) == 1 = Equilateral
  | (length $ nub [a, b, c]) == 2 = Isosceles
  | otherwise = Scalene
  where [x, y, z] = sort [a, b, c]