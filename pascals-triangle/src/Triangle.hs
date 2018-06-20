module Triangle (rows) where

rows :: Int -> [[Integer]]
rows n
  | n <= 0 = []
  | n == 1 = [[1]]
  | otherwise = rows (n - 1) ++ [[1] ++ [x + y | (x, i) <- datas, (y, j) <- datas, j - i == 1] ++ [1]]
  where datas = zip (last $ rows (n - 1)) [1..]
