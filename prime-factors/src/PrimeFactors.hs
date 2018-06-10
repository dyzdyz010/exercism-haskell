module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n = reverse $ primeFactors' n 2 []

primeFactors' :: Integer -> Integer -> [Integer] -> [Integer]
primeFactors' 1 _ list = list
primeFactors' n c list = (if n `mod` c == 0 then
                           (primeFactors' (n `div` c) 2 list) ++ [c] else
                           primeFactors' n (c + 1) list
                         ) ++ list
