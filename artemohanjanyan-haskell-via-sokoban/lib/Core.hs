module Core where

import List

data Coord = C Integer Integer
  deriving (Eq, Show)

data Direction = R | U | L | D
  deriving Eq

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

traverseCoords :: Monoid a => (Coord -> a) -> a
traverseCoords f =
  go21times (\r ->
    go21times (\c ->
      f (C r c)
    )
  )
  where
    go21times something = go (-10)
      where
        go 11 = mempty
        go n  = something n <> go (n+1)

data Tile = Wall | Ground | Storage | Box | Blank
  deriving Eq

type Maze = Coord -> Tile

data Level = Level
  { lStart :: Coord
  , lMaze :: Maze
  }

isClosed :: Level -> Bool
isClosed level = startIsGood && isGraphClosed (lStart level) adjacent isOk
  where
    startTile = lMaze level (lStart level)
    startIsGood = startTile == Ground || startTile == Storage

    adjacent x =
      filterList ((/= Wall) . lMaze level)
      (mapList (`adjacentCoord` x) directions)
    isOk x = lMaze level x /= Blank

    directions = R `Entry` U `Entry` L `Entry` D `Entry` Empty

noBoxMaze :: Maze -> Maze
noBoxMaze maze coord =
  let tile = maze coord
  in if tile == Box then Ground else tile

mazeWithBoxes :: Maze -> List Coord -> Maze
mazeWithBoxes maze boxes coord
  | containsList coord boxes = Box
  | otherwise = noBoxMaze maze coord
