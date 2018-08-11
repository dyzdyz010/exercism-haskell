module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
  | inputBase < 2 = Left InvalidInputBase
  | outputBase < 2 = Left InvalidOutputBase
  | not $ null invalidDigits  = Left $ InvalidDigit $ head invalidDigits
    where invalidDigits = filter (\x -> x >= inputBase || x < 0) inputDigits
rebase inputBase outputBase inputDigits
  | sum inputDigits == 0 = Right []
  | otherwise = Right $ reverse $ decToOut (foldl (\a b -> inputBase*a + b) 0 inputDigits) outputBase

decToOut :: Integral a => a -> a -> [a]
decToOut number base
  | number < base = [number]
  | otherwise = modn: decToOut divn base
  where modn = number `mod` base
        divn = number `div` base