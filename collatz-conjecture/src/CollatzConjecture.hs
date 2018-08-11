module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz x | x <= 0 = Nothing
collatz x = Just $ collatz' x 0

collatz' :: Integer -> Integer -> Integer
collatz' 1 r = r
collatz' x r =
  case even x of
    True -> collatz' (even' x) r+1
    False -> collatz' (odd' x) r+1

even' :: Integer -> Integer
even' x = x `div` 2

odd' :: Integer -> Integer
odd' x = 3*x + 1