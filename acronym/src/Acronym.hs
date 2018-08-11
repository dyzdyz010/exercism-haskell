module Acronym (abbreviate) where

import Data.Char

abbreviate :: String -> String
abbreviate xs = abbreviate' xs True

abbreviate' :: String -> Bool -> String
abbreviate' [] _ = []
abbreviate' (x: y: xs) _
  | isLower x && isUpper y = y : abbreviate' xs False
abbreviate' (x: y: xs) _
  |(not $ isLetter x) && (isLetter y) = toUpper y : abbreviate' xs False
abbreviate' (x: xs) isFirst = if isFirst then toUpper x : abbreviate' xs False else abbreviate' xs False