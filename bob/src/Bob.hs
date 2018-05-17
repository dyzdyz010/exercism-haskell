module Bob (responseFor) where

data Mood = Sure
          | Chillout
          | Calmdown
          | Fine
          | Whatever

responseFor :: String -> String
responseFor xs = responseFor' xs Fine

responseFor' :: String -> Mood -> String
responseFor' [] mood =
  case mood of
    Sure -> "Sure."
    Chillout -> "Whoa, chill out!"
    Calmdown -> "Calm down, I know what I'm doing!"
    Fine -> "Fine. Be that way!"
    Whatever -> "Whatever."

responseFor' [x] mood =
  case mood of
    Fine -> if isNumber x then responseFor' [] Whatever
            else if x == '?' then responseFor' [] Sure
            else responseFor' [] mood
    Whatever -> if x == '?' then responseFor' [] Sure else responseFor' [] mood
    Chillout -> if x == '?' then responseFor' [] Calmdown else responseFor' [] mood
    _ -> responseFor' [] mood

responseFor' (x: xs) mood =
  if x == '?' then responseFor' xs Sure else
    case mood of
      Sure -> if isLowercase x then responseFor' xs Whatever else responseFor' xs mood
      Fine -> if isUppercase x then
                responseFor' xs Chillout
              else if isLowercase x then
                responseFor' xs Whatever
              else
                responseFor' xs mood
      Chillout -> if isLowercase x then
                    responseFor' xs Whatever
                  else
                    responseFor' xs mood
      _ -> responseFor' xs mood


isLowercase :: Char -> Bool
isLowercase x = elem x ['a'..'z']

isUppercase :: Char -> Bool
isUppercase x = elem x ['A'..'Z']

isNumber :: Char -> Bool
isNumber x = elem x ['0'..'9']
