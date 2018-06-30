module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired xs = arePaired' xs []

arePaired' :: String -> String -> Bool
arePaired' [] [] = True
arePaired' [] _ = False
arePaired' (x: xs) ss =
  if x `elem` lb then arePaired' xs (x:ss)
  else if x `elem` rb then
    if ss /= [] && (half $ head ss) == x then arePaired' xs (drop 1 ss) else False
  else arePaired' xs ss
  where lb = "([{"
        rb = ")]}"
        half h = case h of
          '(' -> ')'
          '[' -> ']'
          '{' -> '}'
