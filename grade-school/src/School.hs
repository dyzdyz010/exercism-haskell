module School (School, add, empty, grade, sorted) where

import Data.List(sort, sortOn)

data School = School [(Int, [String])]
instance Show School where show (School s) = show s

add :: Int -> String -> School -> School
add gradeNum student (School []) = School [(gradeNum, [student])]
add gradeNum student (School ((g, ns): xs)) =
  if gradeNum == g then
    School [(g, ns ++ [student])] `concat'` School xs
  else
    School [(g, ns)] `concat'` (add gradeNum student $ School xs)


empty :: School
empty = School []

grade :: Int -> School -> [String]
grade _ (School []) = []
grade gradeNum (School school) = snd $ (filter (\(g, x) -> g == gradeNum) school) !! 0

sorted :: School -> [(Int, [String])]
sorted (School school) = sortOn fst [(x, sort y) | (x, y) <- school]

concat' :: School -> School -> School
(School a) `concat'` (School b) = School (a ++ b)