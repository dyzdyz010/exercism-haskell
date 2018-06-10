module Allergies (Allergen(..), allergies, isAllergicTo) where

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Enum)

allergindex :: Int -> ([Allergen] -> [Allergen])
allergindex n | n > 7 = id
allergindex n = (iterate succ Eggs !! n :)

allergies :: Int -> [Allergen]
allergies 0 = []
allergies score = allergindex factor $ allergies (score - 2 ^ factor)
  where factor = last [x | x <- [0..20], (score - 2^x) `elem` [0..2^x]]

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = allergen `elem` allergies score
