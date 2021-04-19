module State where

import Core
import List
import Mazes

data State = State
  { sPosition :: Coord
  , sDirection :: Direction
  , sBoxes :: List Coord
  , sCurrentLevel :: Integer
  } deriving Eq

initialBoxes :: Level -> List Coord
initialBoxes level = traverseCoords $ \c ->
  if lMaze level c == Box
    then Entry c Empty
    else Empty

initialState :: Integer -> State
initialState levelN = State
  { sPosition = lStart level
  , sDirection = U
  , sBoxes = initialBoxes level
  , sCurrentLevel = levelN
  }
  where
    level = nth levels levelN

stateMaze :: State -> Maze
stateMaze state = lMaze (nth levels (sCurrentLevel state))

movePlayer :: Direction -> State -> State
movePlayer direction state =
  newState { sDirection = direction }
  where
    maze = stateMaze state

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
    maze = stateMaze s

    isOnStorage :: Coord -> Bool
    isOnStorage coord = maze coord == Storage

nextLevel :: State -> Maybe State
nextLevel state
  | sCurrentLevel state < listLength levels =
      Just (initialState (sCurrentLevel state + 1))
  | otherwise = Nothing
