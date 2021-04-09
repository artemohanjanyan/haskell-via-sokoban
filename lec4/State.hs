module State where

import Core
import List

data State = State
  { sPosition :: Coord
  , sDirection :: Direction
  , sBoxes :: List Coord
  } deriving Eq

initialBoxes :: List Coord
initialBoxes = traverseCoords $ \c -> 
  if maze c == Box
    then Entry c Empty
    else Empty

initialState :: State
initialState = State
  { sPosition = C 1 1
  , sDirection = U
  , sBoxes = initialBoxes
  }

movePlayer :: Direction -> State -> State
movePlayer direction state@(State position _ boxes) =
  newState { sDirection = direction }
  where
    isOk :: Tile -> Bool
    isOk Ground = True
    isOk Storage = True
    isOk _ = False

    position' = adjacentCoord direction position
    position'' = adjacentCoord direction position'

    maze' = mazeWithBoxes boxes

    move coord = if coord == position' then position'' else coord
    movedBoxes = mapList move boxes

    newState = 
      if isOk (maze' position')
        then state { sPosition = position' }
      else if maze' position' == Box && isOk (maze' position'')
        then state { sPosition = position', sBoxes = movedBoxes }
      else
        state

isWon :: State -> Bool
isWon (State _ _ boxes) = allList (mapList isOnStorage boxes)
  where
    isOnStorage :: Coord -> Bool
    isOnStorage coord = maze coord == Storage
