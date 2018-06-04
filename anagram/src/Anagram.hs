module Anagram (anagramsFor) where

import Data.List(sort)
import Data.Char(toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter (\x -> (sort $ map toLower x) == (sort $ map toLower xs) && (map toLower x) /= (map toLower xs)) xss
