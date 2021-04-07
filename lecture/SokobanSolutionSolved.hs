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

appendList :: List a -> List a -> List a
appendList Empty a = a
appendList (Entry x xs) a = x `Entry` appendList xs a

containsList :: (a -> a -> Bool) -> a -> List a -> Bool
containsList _ _ Empty = False
containsList (===) x (Entry y ys) = x === y || containsList (===) x ys

-- Coordinates

data Coord = C Integer Integer

data Direction = R | U | L | D

eqCoord :: Coord -> Coord -> Bool
eqCoord (C x1 y1) (C x2 y2) = x1 == x2 && y1 == y2

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

-- The state

data State = State
  { sPosition :: Coord
  , sDirection :: Direction
  , sBoxes :: List Coord
  }

initialBoxes :: List Coord
initialBoxes =
  go21Times (\r ->
    go21Times (\c ->
      let coord = C r c in
      if maze coord `eqTile` Box
        then Entry coord Empty
        else Empty
    )
  )
  where
    go21Times :: (Integer -> List Coord) -> List Coord
    go21Times something = go (-10)
      where
        go :: Integer -> List Coord
        go 11 = Empty
        go n  = appendList (something n) (go (n+1))

initialState :: State
initialState = State
  { sPosition = C 0 1
  , sDirection = U
  , sBoxes = initialBoxes
  }

-- The maze

data Tile = Wall | Ground | Storage | Box | Blank

eqTile :: Tile -> Tile -> Bool
eqTile Wall Wall = True
eqTile Ground Ground = True
eqTile Storage Storage = True
eqTile Box Box = True
eqTile Blank Blank = True
eqTile _ _ = False

maze :: Coord -> Tile
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

noBoxMaze :: Coord -> Tile
noBoxMaze coord = case maze coord of
  Box -> Ground
  tile -> tile

mazeWithBoxes :: List Coord -> Coord -> Tile
mazeWithBoxes boxes coord
  | containsList eqCoord coord boxes = Box
  | otherwise = noBoxMaze coord

-- Event handling

handleEvent :: Event -> State -> State
handleEvent _ s | isWon s = s
handleEvent (KeyPress key) s
  | key == "Right" = handle R
  | key == "Up" = handle U
  | key == "Left" = handle L
  | key == "Down" = handle D
  where
    handle direction =
      let nextCoord' = adjacentCoord direction (sPosition s)
          nextCoord'' = adjacentCoord direction nextCoord'

          maze' = mazeWithBoxes (sBoxes s)

          available coord = isOk (maze' coord)

          moveBox c = if c `eqCoord` nextCoord' then nextCoord'' else c
          newBoxes = mapList moveBox (sBoxes s)

          newS = if available nextCoord'
            then s { sPosition = nextCoord' }
            else if maze' nextCoord' `eqTile` Box && available nextCoord''
              then s
                { sPosition = nextCoord'
                , sBoxes = newBoxes
                }
              else s

      in newS { sDirection = direction }

    isOk :: Tile -> Bool
    isOk Ground = True
    isOk Storage = True
    isOk _ = False 
handleEvent _ s = s

isWon :: State -> Bool
isWon s = allList (mapList isOnStorage (sBoxes s))

isOnStorage :: Coord -> Bool
isOnStorage c = maze c `eqTile` Storage

allList :: List Bool -> Bool
allList bools = not (containsList (==) False bools)

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
drawTileAt c = atCoord c (drawTile (noBoxMaze c))


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
drawState s =
  (if isWon s then scaled 3 3 (lettering "Haskell!") else blank) &
  atCoord (sPosition s) (player (sDirection s)) &
  pictureOfBoxes (sBoxes s) &
  pictureOfMaze

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
main = runActivity (resetable (withStartScreen sokoban))
