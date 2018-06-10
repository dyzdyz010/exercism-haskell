module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n = primeFactors' n 2 []

primeFactors' :: Integer -> Integer -> [Integer] -> [Integer]
primeFactors' 1 _ list = list
primeFactors' n c list = list ++
                         (if n `mod` c == 0 then
                            c:  (primeFactors' (n `div` c) 2 list)
                          else
                            primeFactors' n (c + 1) list
                         )
