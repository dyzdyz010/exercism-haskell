module Series (slices) where

import           Data.Char (digitToInt)

slices :: Int -> String -> [[Int]]
slices n xs = [map digitToInt $ take n $ drop x xs | x <- [0..length xs - n]]