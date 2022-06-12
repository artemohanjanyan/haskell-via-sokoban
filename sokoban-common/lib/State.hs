module State where

import Core
import List

data State = State
  { sPosition :: Coord
  , sDirection :: Direction
  , sBoxes :: List Coord
  , sCurrentLevel :: Level
  }

instance Eq State where
  (State p1 d1 b1 _) == (State p2 d2 b2 _) = p1 == p2 && d1 == d2 && b1 == b2

sMaze :: State -> Maze
sMaze = lMaze . sCurrentLevel

initialBoxes :: Level -> List Coord
initialBoxes level = traverseCoords $ \c ->
  if lMaze level c == Box
    then Entry c Empty
    else Empty

initialState :: Level -> State
initialState level = State
  { sPosition = lStart level
  , sDirection = U
  , sBoxes = initialBoxes level
  , sCurrentLevel = level
  }

movePlayer :: Direction -> State -> State
movePlayer direction state =
  newState { sDirection = direction }
  where
    maze = sMaze state

    isOk :: Tile -> Bool
    isOk Ground = True
    isOk Storage = True
    isOk _ = False

    position' = adjacentCoord direction (sPosition state)
    position'' = adjacentCoord direction position'

    maze' = mazeWithBoxes maze (sBoxes state)

    move coord = if coord == position' then position'' else coord
    movedBoxes = mapList move (sBoxes state)

    newState = 
      if isOk (maze' position')
        then state { sPosition = position' }
      else if maze' position' == Box && isOk (maze' position'')
        then state { sPosition = position', sBoxes = movedBoxes }
      else
        state

isLevelComplete :: State -> Bool
isLevelComplete s = allList (mapList isOnStorage (sBoxes s))
  where
    maze = sMaze s

    isOnStorage :: Coord -> Bool
    isOnStorage coord = maze coord == Storage
