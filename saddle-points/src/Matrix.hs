module Matrix (saddlePoints) where

import           Data.Array

saddlePoints :: (Ix a, Ord e) => Array (a, a) e -> [(a, a)]
saddlePoints matrix = [(x, y) |
                       x <- range (rz, rn),
                       y <- range (cz, cn),
                       (maximum $ row x) == (matrix ! (x, y)),
                       (minimum $ col y) == (matrix ! (x, y))]
  where ((rz, cz), (rn, cn)) = bounds matrix
        row n = [matrix ! (n, x) | x <- range (cz, cn)]
        col n = [matrix ! (x, n) | x <- range (rz, rn)]