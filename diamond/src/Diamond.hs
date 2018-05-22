module Diamond (diamond) where

diamond :: Char -> Maybe [String]
diamond c | not $ c `elem` ['A'..'Z'] = Nothing
diamond c = Just $ (half c) ++ [row c c] ++ (reverse $ half c)

half :: Char -> [String]
half c =  [row x c | x <- ['A'..pred c]]

row :: Char -> Char -> String
row x c = (replicate (length [succ x .. c])  ' ')
           ++ [x]
           ++ (if x=='A' then [] else ( (replicate ((length  ['A'..x] - 2) * 2 + 1) ' ') ++ [x]))
           ++ (replicate (length [succ x .. c])  ' ')
