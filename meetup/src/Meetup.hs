module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = wmodifier schedule weekday year month
  $ filter (\d -> (wd d) == wconvert weekday)
  [h x | x <- [1..gregorianMonthLength year month]]
  where h = fromGregorian year month
        wd d = formatTime defaultTimeLocale "%a" d

wconvert :: Weekday -> String
wconvert str = case str of
  Monday -> "Mon"
  Tuesday -> "Tue"
  Wednesday -> "Wed"
  Thursday -> "Thu"
  Friday -> "Fri"
  Saturday -> "Sat"
  Sunday -> "Sun"

wmodifier :: Schedule -> Weekday -> Integer -> Int -> [Day] -> Day
wmodifier sch weekday year month days =
  case sch of
    First -> days !! 0
    Second -> days !! 1
    Third -> days !! 2
    Fourth -> days !! 3
    Last -> last days
    Teenth -> filter (\d -> dayd d `elem` [13..19]) days !! 0
  where dayd d = read (formatTime defaultTimeLocale "%d" d)::Int
