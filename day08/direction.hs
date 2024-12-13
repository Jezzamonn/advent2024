module Direction (
    Direction(..),
    turnLeft,
    turnRight,
    delta
) where

import Prelude hiding (Left, Right)

data Direction = Up | Left | Down | Right
  deriving (Show, Eq, Enum, Bounded)

turnLeft :: Direction -> Direction
turnLeft Up    = Left
turnLeft Left  = Down
turnLeft Down  = Right
turnLeft Right = Up

turnRight :: Direction -> Direction
turnRight Up    = Right
turnRight Right = Down
turnRight Down  = Left
turnRight Left  = Up

-- In (x, y) order.
delta :: Direction -> (Int, Int)
delta Up    = (0, -1)
delta Left  = (-1, 0)
delta Down  = (0, 1)
delta Right = (1, 0)