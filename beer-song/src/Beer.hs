module Beer (song) where

song :: String
song = song' 99

song' :: Int -> String
song' 0 = "No more bottles of beer on the wall, no more bottles of beer.\n\
          \Go to the store and buy some more, 99 bottles of beer on the wall.\n"
song' 1 = "1 bottle of beer on the wall, 1 bottle of beer.\n\
          \Take it down and pass it around, no more bottles of beer on the wall.\n" ++ "\n" ++ song' 0
song' i = bottles i ++ " of beer on the wall, " ++ bottles i ++ " of beer.\n\
          \Take one down and pass it around, " ++ bottles (i - 1) ++ " of beer on the wall.\n" ++ "\n" ++ song' (i - 1)
          where bottles i = show i ++ " bottle" ++ (if i == 1 then ""  else "s")
