module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify x
  | x <= 0 = Nothing
  | diff < 0 = Just Deficient
  | diff == 0 = Just Perfect
  | diff > 0 = Just Abundant
  where diff = sum [a | a <- [1..x-1], x `mod` a == 0] - x
