module Isogram (isIsogram) where

import Data.List(sort)
import Data.Char(isLetter, toLower)

isIsogram :: String -> Bool
isIsogram s = isIsogram' (sort [toLower x | x <- s, isLetter x]) ' '

isIsogram' :: String -> Char -> Bool
isIsogram' [] _ = True
isIsogram' (x: xs) last = if x == last then False else isIsogram' xs x
