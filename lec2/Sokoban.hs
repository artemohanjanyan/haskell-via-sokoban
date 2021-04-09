{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

-- Basic types --

data Tile = Wall | Ground | Storage | Box | Blank
  deriving Eq

data Coord = Coord Int Int
  deriving Eq

data Direction = R | U | L | D

-- Pictures --

rgb :: Double -> Double -> Double -> Color
rgb r g b = RGB (r / 256) (g / 256) (b / 256)

wall, ground, storage, box :: Picture
wall = horizontal & vertical & background
  where
    horizontal =
      polyline [(0.5, 0.5), (-0.5, 0.5)] &
      polyline [(0.5, 0.25), (-0.5, 0.25)] &
      polyline [(0.5, 0), (-0.5, 0)] &
      polyline [(0.5, -0.25), (-0.5, -0.25)] &
      polyline [(0.5, -0.5), (-0.5, -0.5)]
    vertical =
      polyline [(-0.5, 0.5), (-0.5, 0.25)] &
      polyline [(-0.5, 0), (-0.5, -0.25)] &
      polyline [(-0.25, 0.25), (-0.25, 0)] &
      polyline [(-0.25, -0.25), (-0.25, -0.5)] &
      polyline [(0, 0.5), (0, 0.25)] &
      polyline [(0, 0), (0, -0.25)] &
      polyline [(0.25, 0.25), (0.25, 0)] &
      polyline [(0.25, -0.25), (0.25, -0.5)]
    background =
      colored (rgb 161 149 85) (solidRectangle 1 1)
ground = colored (rgb 222 214 174) (solidRectangle 1 1)
storage = colored (rgb 215 149 133) (solidCircle 0.125) & ground
box =
  square 0.5 &
  square 0.35 &
  polygon diagonal &
  colored (dark color) (solidPolygon diagonal) &
  colored (darker 0.25 color) (solidPolygon
    [ (-0.35, -0.35)
    , (-0.28, -0.28)
    , (-0.28, 0.28)
    , (0.28, 0.28)
    , (0.35, 0.35)
    , (-0.35, 0.35)
    ]) &
  colored (light color) (solidPolygon
    [ (-0.35, -0.35)
    , (-0.28, -0.42)
    , (0.42, -0.42)
    , (0.42, 0.28)
    , (0.35, 0.35)
    , (0.35, -0.35)
    ]) &
  colored (light color) (solidPolygon
    [ (-0.43, -0.43)
    , (-0.5, -0.5)
    , (-0.5, 0.5)
    , (0.5, 0.5)
    , (0.43, 0.43)
    , (-0.43, 0.43)
    ]) &
  colored color (solidRectangle 1 1)
  where
    color = rgb 241 174 66
    square r = polygon [(-r, -r), (-r, r), (r, r), (r, -r)]
    diagonal =
      [ (-0.35, -0.35)
      , (-0.35, -0.25)
      , (0.25, 0.35)
      , (0.35, 0.35)
      , (0.35, 0.25)
      , (-0.25, -0.35)
      ]

player :: Direction -> Picture
player direction = arms & body
  where
    body =
      polyline [(-0.1, -0.5), (0, -0.25)] &
      polyline [(0.1, -0.5), (0, -0.25)] &
      polyline [(0, -0.25), (0, 0.1)] &
      translated 0 0.25 (circle 0.15)
    arms = case direction of
      R ->
        polyline [(0.25, 0.1), (0, 0)] &
        polyline [(0.25, -0.1), (0, 0)]
      U ->
        polyline [(-0.25, 0.1), (0, 0)] &
        polyline [(0.25, 0.1), (0, 0)]
      L ->
        polyline [(-0.25, 0.1), (0, 0)] &
        polyline [(-0.25, -0.1), (0, 0)]
      D ->
        polyline [(-0.1, -0.25), (0, 0)] &
        polyline [(0.1, -0.25), (0, 0)]

-- Maze --

maze :: Coord -> Tile
maze (Coord x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

noBoxMaze :: Coord -> Tile
noBoxMaze coord = if tile == Box then Ground else tile
  where
    tile = maze coord

mazeWithBoxes :: [Coord] -> (Coord -> Tile)
mazeWithBoxes boxes coord =
  if any (== coord) boxes
    then Box
    else noBoxMaze coord

-- Drawing --

drawTile :: Tile -> Picture
drawTile Wall = wall
drawTile Ground = ground
drawTile Storage = storage
drawTile Box = box
drawTile Blank = blank

atCoord :: Coord -> Picture -> Picture
atCoord (Coord x y) = translated (fromIntegral x) (fromIntegral y)

coords :: [Coord]
coords = [Coord x y | x <- [-10..10], y <- [-10..10]]

pictureOfMaze :: Picture
pictureOfMaze = mconcat (reverse tiles)
  where
    tiles = [atCoord coord (drawTile (noBoxMaze coord)) | coord <- coords]

pictureOfBoxes :: [Coord] -> Picture
pictureOfBoxes boxes = mconcat $ map (`atCoord` box) boxes

-- Moving --

move :: Direction -> Coord -> Coord
move R (Coord x y) = Coord (x + 1)  y
move U (Coord x y) = Coord  x      (y + 1)
move L (Coord x y) = Coord (x - 1) y
move D (Coord x y) = Coord  x      (y - 1)

available :: Tile -> Bool
available Ground = True
available Storage = True
available _ = False

-- State --

data State = State
  { sPosition :: Coord
  , sDirection :: Direction
  , sBoxes :: [Coord]
  }

initialState :: State
initialState = State (Coord 0 1) D boxes
  where
    boxes = filter (\coord -> maze coord == Box) coords

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) s
  | key == "Right" = handle R
  | key == "Up"    = handle U
  | key == "Left"  = handle L
  | key == "Down"  = handle D
  where
    handle direction = newS { sDirection = direction }
      where
        position' = move direction (sPosition s)
        position'' = move direction position'
        moveBox coord = if coord == position' then position'' else coord

        newS =
          if isWon s
            then s
            else if available (mazeWithBoxes (sBoxes s) position')
              then s { sPosition = position' }
              else if (any (== position') (sBoxes s) &&
                       available (mazeWithBoxes (sBoxes s) position''))
                then s
                  { sPosition = position'
                  , sBoxes = map moveBox (sBoxes s)
                  }
                else s
handleEvent _ c = c

drawState :: State -> Picture
drawState s =
  (if isWon s then scaled 3 3 (lettering "Haskell!") else blank) &
  atCoord (sPosition s) (player (sDirection s)) &
  pictureOfBoxes (sBoxes s) &
  pictureOfMaze

isWon :: State -> Bool
isWon s = all ((== Storage) . noBoxMaze) (sBoxes s)

-- Activity --

data Activity state = Activity
  state
  (Event -> state -> state)
  (state -> Picture)

resetable :: Activity state -> Activity state
resetable (Activity state0 handle draw) = Activity state0 handle' draw
  where
    handle' (KeyPress "Esc") _ = state0
    handle' event state = handle event state

startScreen :: Picture
startScreen = scaled 3 3 (lettering "Sokoban!")

data StartScreenState state = StartScreen | Running state

withStartScreen :: Activity state -> Activity (StartScreenState state)
withStartScreen (Activity state0 handle draw) = Activity state0' handle' draw'
  where
    state0' = StartScreen

    handle' (KeyPress " ") StartScreen = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s

runActivity :: Activity state -> IO ()
runActivity (Activity state0 handle draw) = activityOf state0 handle draw

-- Main --

sokoban :: Activity State
sokoban = Activity initialState handleEvent drawState

main :: IO ()
main = runActivity (resetable (withStartScreen sokoban))
