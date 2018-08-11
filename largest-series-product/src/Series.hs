module Series (Error(..), largestProduct) where

import Data.Char(isDigit, digitToInt)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size < 0 || size > length digits = Left InvalidSpan
  | not $ null $ nonDigits = Left $ InvalidDigit $ head nonDigits
  where nonDigits = filter (not . isDigit) digits
largestProduct 0 _ = Right 1
largestProduct size digits = Right $ toInteger $ maximum $ candidates size digits

candidates :: Int -> String -> [Int]
candidates size digits
  | size == length digits = [product $ map digitToInt digits]
  | otherwise = (product $ map digitToInt $ take size digits): candidates size (drop 1 digits)