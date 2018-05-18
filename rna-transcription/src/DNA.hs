module DNA (toRNA) where

toRNA :: String -> Maybe String
toRNA xs
  | length [a | a <- xs, not $ a `elem` ['G', 'C', 'T', 'A']] > 0 = Nothing
toRNA (xs) = Just $ map transcript xs

transcript :: Char -> Char
transcript x =
  case x of
    'G' -> 'C'
    'C' -> 'G'
    'T' -> 'A'
    'A' -> 'U'
