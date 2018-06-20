module House (rhyme) where

import           Text.Printf

rhyme :: String
rhyme = (foldl (++) "" $ reverse $ paragraphs 11) ++ "\n"

paragraphs :: Int -> [String]
paragraphs n
  | n == 0 = [sent 0]
  | otherwise = sent n : "\n\n" : paragraphs (n - 1)
  where sent x = (foldl (++) "" $ reverse $ sentences x x) ++ "."

sentences :: Int -> Int -> [String]
sentences 0 s = [printf "This is %s" (items !! s)]
sentences n s = printf "that %s %s" (actions !! (s - n)) (items !! (s - n)) : "\n" : sentences (n - 1) s

items :: [String]
items = ["the house that Jack built",
         "the malt",
         "the rat",
         "the cat",
         "the dog",
         "the cow with the crumpled horn",
         "the maiden all forlorn",
         "the man all tattered and torn",
         "the priest all shaven and shorn",
         "the rooster that crowed in the morn",
         "the farmer sowing his corn",
         "the horse and the hound and the horn"]

actions :: [String]
actions = ["lay in",
           "ate",
           "killed",
           "worried",
           "tossed",
           "milked",
           "kissed",
           "married",
           "woke",
           "kept",
           "belonged to",
           "is"]
