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

maze :: Coord -> Tile
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

noBoxMaze :: Coord -> Tile
noBoxMaze coord =
  let tile = maze coord
  in if tile == Box then Ground else tile

mazeWithBoxes :: List Coord -> Coord -> Tile
mazeWithBoxes boxes coord
  | containsList coord boxes = Box
  | otherwise = noBoxMaze coord
