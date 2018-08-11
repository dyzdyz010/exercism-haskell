module Isogram (isIsogram) where

import Data.List(elemIndices)
import Data.Char(isLetter, toLower)

isIsogram :: String -> Bool
isIsogram s =
  all (\x -> (length $ elemIndices x [toLower x | x <- s, isLetter x]) <= 1) ['a'..'z']