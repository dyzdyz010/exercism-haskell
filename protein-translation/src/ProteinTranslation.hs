module ProteinTranslation(proteins) where

proteins :: String -> Maybe [String]
proteins xs = Just $ proteins' xs

proteins' :: String -> [String]
proteins' [] = []
proteins' (f: s: t: xs) | [f, s, t] `elem` ["UAA", "UAG", "UGA"] = []
proteins' (f: s: t: xs) | [f, s, t] `elem` ["AUG"] = "Methionine": proteins' xs
proteins' (f: s: t: xs) | [f, s, t] `elem` ["UUU", "UUC"] = "Phenylalanine": proteins' xs
proteins' (f: s: t: xs) | [f, s, t] `elem` ["UUA", "UUG"] = "Leucine": proteins' xs
proteins' (f: s: t: xs) | [f, s, t] `elem` ["UCU", "UCC", "UCA", "UCG"] = "Serine": proteins' xs
proteins' (f: s: t: xs) | [f, s, t] `elem` ["UAU", "UAC"] = "Tyrosine": proteins' xs
proteins' (f: s: t: xs) | [f, s, t] `elem` ["UGU", "UGC"] = "Cysteine": proteins' xs
proteins' (f: s: t: xs) | [f, s, t] `elem` ["UGG"] = "Tryptophan": proteins' xs
