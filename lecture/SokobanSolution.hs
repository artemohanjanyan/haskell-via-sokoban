{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- Lists

data List a = Empty | Entry a (List a)

mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

combine :: List Picture -> Picture
combine Empty = blank
combine (Entry p ps) = p & combine ps

-- Coordinates

data Coord = C Integer Integer

data Direction = R | U | L | D

eqCoord :: Coord -> Coord -> Bool
eqCoord = undefined

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

-- The state

data State = State -- FIXME!

initialBoxes :: List Coord
initialBoxes = undefined

initialState :: State
initialState = State -- FIXME!

-- The maze

data Tile = Wall | Ground | Storage | Box | Blank

maze :: Coord -> Tile
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

noBoxMaze :: Coord -> Tile
noBoxMaze = undefined

mazeWithBoxes :: List Coord -> Coord -> Tile
mazeWithBoxes = undefined

-- Event handling

handleEvent :: Event -> State -> State
--handleEvent (KeyPress key) c
--  | key == "Right" = tryGoTo c R
--  | key == "Up"    = tryGoTo c U
--  | key == "Left"  = tryGoTo c L
--  | key == "Down"  = tryGoTo c D
--  where
--    tryGoTo :: State -> Direction -> State
--    tryGoTo (State from _) d
--      | isOk (maze to) = State to d
--      | otherwise      = State from d
--      where to = adjacentCoord d from
--
--    isOk :: Tile -> Bool
--    isOk Ground = True
--    isOk Storage = True
--    isOk _ = False
handleEvent _ c = c

isWon :: State -> Bool
isWon = undefined

isOnStorage :: Coord -> Bool
isOnStorage = undefined

allList :: List Bool -> Bool
allList = undefined

-- Drawing

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

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

pictureOfMaze :: Picture
pictureOfMaze = draw21times (\r -> draw21times (\c -> drawTileAt (C r c)))
  where
    draw21times :: (Integer -> Picture) -> Picture
    draw21times something = go (-10)
      where
        go :: Integer -> Picture
        go 11 = blank
        go n  = something n & go (n+1)

drawTileAt :: Coord -> Picture
drawTileAt c = atCoord c (drawTile (maze c))


atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic


player :: Direction -> Picture
player R = translated 0 0.3 cranium
         & polyline [(0,0),(0.3,0.05)]
         & polyline [(0,0),(0.3,-0.05)]
         & polyline [(0,-0.2),(0,0.1)]
         & polyline [(0,-0.2),(0.1,-0.5)]
         & polyline [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & sector (7/6*pi) (1/6*pi) 0.18
player L = scaled (-1) 1 (player R) -- Cunning!
player U = translated 0 0.3 cranium
         & polyline [(0,0),(0.3,0.05)]
         & polyline [(0,0),(-0.3,0.05)]
         & polyline [(0,-0.2),(0,0.1)]
         & polyline [(0,-0.2),(0.1,-0.5)]
         & polyline [(0,-0.2),(-0.1,-0.5)]
  where cranium = solidCircle 0.18
player D = translated 0 0.3 cranium
         & polyline [(0,0),(0.3,-0.05)]
         & polyline [(0,0),(-0.3,-0.05)]
         & polyline [(0,-0.2),(0,0.1)]
         & polyline [(0,-0.2),(0.1,-0.5)]
         & polyline [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & translated   0.06  0.08 (solidCircle 0.04)
                & translated (-0.06) 0.08 (solidCircle 0.04)

pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes cs = combine (mapList (\c -> atCoord c (drawTile Box)) cs)

drawState :: State -> Picture
drawState State = pictureOfMaze

-- The complete activity

sokoban :: Activity State
sokoban = Activity initialState handleEvent drawState

-- The general activity type

data Activity world = Activity
        world
        (Event -> world -> world)
        (world -> Picture)


runActivity :: Activity s -> IO ()
runActivity (Activity state0 handle draw)
  = activityOf state0 handle draw

-- Resetable activities

resetable :: Activity s -> Activity s
resetable (Activity state0 handle draw)
  = Activity state0 handle' draw
  where handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s = handle e s

-- Start screen

startScreen :: Picture
startScreen = scaled 3 3 (lettering "Sokoban!")

data SSState world = StartScreen | Running world

withStartScreen :: Activity s  -> Activity (SSState s)
withStartScreen (Activity state0 handle draw)
  = Activity state0' handle' draw'
  where
    state0' = StartScreen

    handle' (KeyPress key) StartScreen | key == " " = Running state0
    handle' _              StartScreen              = StartScreen
    handle' e              (Running s)              = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s


-- The main function

main :: IO ()
main = runActivity sokoban
