module Phone (number) where

import Data.Char(isNumber)

number :: String -> Maybe String
number xs = case length nums of
  11 -> let (a: b: _: _: c: _) = nums in if validac a && validec b c then Just $ drop 1 nums else Nothing
  10 -> let (b: _: _: c: _) = nums in if validec b c then Just nums else Nothing
  otherwise -> Nothing
  where nums = filter isNumber xs
        validac a = a == '1'
        validec a b = all (`elem` ['2'..'9']) [a, b]
