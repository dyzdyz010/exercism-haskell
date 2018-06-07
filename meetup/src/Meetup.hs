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
meetupDay schedule weekday year month = head $ filter (wmodifier schedule weekday year month) [h x | x <- [1..31]]
  where h = fromGregorian year month

wconvert :: Weekday -> String
wconvert str = case str of
  Monday -> "Mon"
  Tuesday -> "Tue"
  Wednesday -> "Wed"
  Thursday -> "Thu"
  Friday -> "Fri"
  Saturday -> "Sat"
  Sunday -> "Sun"

wmodifier :: Schedule -> Weekday -> Integer -> Int -> (Day -> Bool)
wmodifier sch weekday year month = (\d -> weq d &&  dayd d `elem`
                                        (case sch of
                                          First -> [1..7]
                                          Second -> [8..14]
                                          Third -> [15..21]
                                          Fourth -> [22..28]
                                          Last -> [mlen-6 .. mlen]
                                          Teenth -> [13..19]
                                        )
                                   )
  where wd d = formatTime defaultTimeLocale "%a" d
        weq d = (wd d == wconvert weekday)
        dayd d = read (formatTime defaultTimeLocale "%d" d)::Int
        mon d = formatTime defaultTimeLocale "%m" d
        mlen = gregorianMonthLength year month
