module Raindrops (convert) where

convert :: Int -> String
convert n = if length factors == 0 then
              show n
            else
              foldl (\l x -> l ++ convert' x) [] factors
  where factors = filter (\x -> n `mod` x == 0) [3, 5, 7]

convert' :: Int -> String
convert' n
  | n == 3 = "Pling"
  | n == 5 = "Plang"
  | n == 7 = "Plong"
