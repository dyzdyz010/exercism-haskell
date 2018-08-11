module DNA (nucleotideCounts) where

import Data.Map (Map, fromList)

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts xs | length [x | x <- xs, not $ x `elem` "ACGT"] > 0 = Left "Invalid"
nucleotideCounts xs = Right $ fromList $ zip "ACGT" [length [x | x <- xs, x=='A'],
                                                     length [x | x <- xs, x=='C'],
                                                     length [x | x <- xs, x=='G'],
                                                     length [x | x <- xs, x=='T']]