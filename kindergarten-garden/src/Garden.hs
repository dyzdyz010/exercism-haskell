module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.List(elemIndex)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

data Garden = Garden ([String], [String])

garden :: [String] -> String -> Garden
garden students plants = Garden (students, lines plants)

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden (students, plants)) = concat $ map (\x -> map flower [x !! (si * 2), x !! (si * 2 + 1)]) plants
  where Just si = student `elemIndex` students

flower :: Char -> Plant
flower f = case f of
             'C' -> Clover
             'G' -> Grass
             'R' -> Radishes
             'V' -> Violets