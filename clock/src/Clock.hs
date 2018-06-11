module Clock (clockHour, clockMin, fromHourMin, toString) where

data Clock = Clock Int deriving(Eq)

instance Num Clock where
  (Clock a) + (Clock b) = Clock (a + b)
  negate (Clock a) = Clock $ daymin - a `mod` daymin
    where daymin = 24 * 60
  fromInteger a = Clock (fromInteger a)

instance Show Clock where
  show (Clock m) = show m

clockHour :: Clock -> Int
clockHour (Clock minutes) = minutes `div` 60 `mod` 24

clockMin :: Clock -> Int
clockMin (Clock minutes) = minutes `mod` 60

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock $ (hour * 60 + min) `mod` (24 * 60)

toString :: Clock -> String
toString clock = hzero ++ (show $ clockHour clock) ++ ":" ++ mzero ++ (show $ clockMin clock)
  where hzero = if clockHour clock < 10 then "0" else ""
        mzero = if clockMin clock < 10 then "0" else ""
