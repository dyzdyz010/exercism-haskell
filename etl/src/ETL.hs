module ETL (transform) where

import Data.Map (Map, fromList, toList)
import Data.Char (toLower)

transform :: Map a String -> Map Char a
transform legacyData = fromList [(toLower x, n) | (n, xs) <- (toList legacyData), x <- xs]