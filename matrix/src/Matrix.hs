module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where

import           Data.Vector (Vector)
import qualified Data.Vector as V (fromList, toList)

data Matrix a = Matrix [[a]] deriving (Eq, Show)

cols :: Matrix a -> Int
cols (Matrix matrix) = length $ (if null matrix then [] else head matrix)

column :: Int -> Matrix a -> Vector a
column x (Matrix matrix) = V.fromList $ map (!! x) matrix

flatten :: Matrix a -> Vector a
flatten (Matrix matrix) = V.fromList $ foldl (++) [] matrix

fromList :: [[a]] -> Matrix a
fromList xss = Matrix xss

fromString :: Read a => String -> Matrix a
fromString xs = fromList $ map (map read) (map words $ lines xs)

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) (Matrix matrix) = fromList $ split (r, c) (last matrix)
  where split (rn, cn) xs
          | rn == 0 = []
          | otherwise = (take cn xs) : (split (rn-1, cn) (drop c xs))

row :: Int -> Matrix a -> Vector a
row x (Matrix matrix) = V.fromList $  matrix !! x

rows :: Matrix a -> Int
rows (Matrix matrix) = length matrix

shape :: Matrix a -> (Int, Int)
shape matrix = (rows matrix, cols matrix)

transpose :: Matrix a -> Matrix a
transpose matrix = fromList [V.toList $ column x matrix | x <- [0..cols matrix - 1]]
