module Sieve (primesUpTo) where

primesUpTo :: Integer -> [Integer]
primesUpTo n = primes [2..n]

primes :: [Integer] -> [Integer]
primes [] = []
primes xs = curr : primes [x | x <- drop 1 xs, x `mod` curr /= 0]
  where curr = head xs