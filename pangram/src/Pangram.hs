module Pangram (isPangram) where
import Data.List(elemIndex)
import Data.Char(toLower)

isPangram :: String -> Bool
isPangram text = isPangram' (letters [toLower x | x <- text]) $ replicate 26 1


isPangram' :: String -> [Int] -> Bool
isPangram' [] list = (sum list) ==0
isPangram' (x:xs) list =
  isPangram' xs $ replace list (let (Just i) = elemIndex x ['a'..'z'] in i) 0


replace :: [a] -> Int -> a -> [a]
replace [] _ _ = []
replace list index r = replace' list index r 0

replace' :: [a] -> Int -> a -> Int -> [a]
replace' [] _ _ _ = []
replace' (x:xs) index r counter = if counter == index then
                                    r:xs
                                  else
                                    x:replace' xs index r (counter+1)

letters :: String -> String
letters [] = []
letters (x:xs) = if elem x (['a'..'z']++['A'..'Z']) then x:letters xs else letters xs
