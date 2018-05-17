module SpaceAge (Planet(..), ageOn) where

import Text.Printf

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds =
  case planet of
    Mercury -> read (printf "%.2f" (earthyear / 0.2408467) :: String) :: Float
    Venus -> read (printf "%.2f" (earthyear / 0.61519726) :: String) :: Float
    Earth -> read (printf "%.2f" earthyear :: String) :: Float
    Mars -> read (printf "%.2f" (earthyear / 1.8808158) :: String) :: Float
    Jupiter -> read (printf "%.2f" (earthyear / 11.862615) :: String) :: Float
    Saturn -> read (printf "%.2f" (earthyear / 29.447498) :: String) :: Float
    Uranus -> read (printf "%.2f" (earthyear / 84.016846) :: String) :: Float
    Neptune -> read (printf "%.2f" (earthyear / 164.79132) :: String) :: Float
  where earthyear = seconds / 31557600
