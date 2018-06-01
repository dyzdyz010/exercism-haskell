module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , simulate
    , turnLeft
    , turnRight
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

data Robot = Robot (Bearing, (Integer, Integer))

bearing :: Robot -> Bearing
bearing (Robot (dir, _)) = dir

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot (_, coords)) = coords

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction coords = Robot (direction, coords)

simulate :: Robot -> String -> Robot
simulate robot [] = robot
simulate robot (x: xs) = case x of
  'A' -> simulate (advance robot) xs
  'L' -> simulate (Robot (turnLeft dir, coor)) xs
  'R' -> simulate (Robot (turnRight dir, coor)) xs
  where (Robot (dir, coor)) = robot

turnLeft :: Bearing -> Bearing
turnLeft direction = case direction of
  North -> West
  East -> North
  South -> East
  West -> South

turnRight :: Bearing -> Bearing
turnRight direction = case direction of
  North -> East
  East -> South
  South -> West
  West -> North

advance :: Robot -> Robot
advance (Robot (dir, (x, y))) = case dir of
  North -> Robot(dir, (x, y+1))
  East -> Robot(dir, (x+1, y))
  South -> Robot(dir, (x, y-1))
  West -> Robot(dir, (x-1, y))
