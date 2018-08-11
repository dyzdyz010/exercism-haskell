module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired xs = arePaired' xs []

arePaired' :: String -> String -> Bool
arePaired' [] [] = True
arePaired' [] _ = False
arePaired' (x: xs) zs
  | isLeft x = arePaired' xs (x: zs)
arePaired' (x: xs) (z: zs)
  | half x == Just z  = arePaired' xs zs
  | half x == Nothing  = arePaired' xs (z: zs)
  | otherwise = False
arePaired' (x: xs) zs
  | half x /= Nothing && null zs = False
  | otherwise = arePaired' xs zs

isLeft :: Char -> Bool
isLeft '(' = True
isLeft '[' = True
isLeft '{' = True
isLeft _ = False

half :: Char -> Maybe Char
half ')' = Just '('
half ']' = Just '['
half '}' = Just '{'
half _ = Nothing
