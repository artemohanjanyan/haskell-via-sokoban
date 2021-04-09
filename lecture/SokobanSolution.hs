{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- import Prelude hiding ((==))

-- import Data.Fixed (mod')
-- import qualified Data.Fixed

-- Lists

data List a = Empty | Entry a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
  (<>) = appendList

instance Monoid (List a) where
  mempty = Empty

-- instance Eq a => Eq (List a) where
--   Empty == Empty = True
--   (Entry x xs) == (Entry y ys) = x == y && xs == ys
--   _ == _ = False

mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

combine :: Monoid a => List a -> a
combine Empty = mempty
combine (Entry p ps) = p <> combine ps

appendList :: List a -> List a -> List a
appendList Empty ys = ys
appendList (Entry x xs) ys = x `Entry` appendList xs ys

containsList :: Eq a => a -> List a -> Bool
containsList _ Empty = False
containsList x (Entry y ys) = x == y || containsList x ys

allList :: List Bool -> Bool
-- allList bools = not (containsList (==) False bools)
allList = not . containsList False

-- Coordinates

data Coord = C Integer Integer
  deriving (Eq, Show)

-- Type wrapper without any runtime overhead
-- newtype Coord' = Coord' Coord

-- Type synonym, doesn't actually create a different type
-- type Coord'' = (Integer, Integer)

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

-- The state

--data State = State Coord Direction (List Coord)
--  deriving Eq

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

-- The maze

data Tile = Wall | Ground | Storage | Box | Blank
  deriving Eq

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

-- Event handling

handleEvent :: Event -> State -> State
handleEvent _ state | isWon state = state
handleEvent (KeyPress key) s@(State position _ boxes)
  | key == "Right" = go R
  | key == "Up"    = go U
  | key == "Left"  = go L
  | key == "Down"  = go D
  where
    go :: Direction -> State
    go direction =
      let position' = adjacentCoord direction position
          position'' = adjacentCoord direction position'

          maze' = mazeWithBoxes boxes

          move coord = if coord == position' then position'' else coord
          movedBoxes = mapList move boxes

          newS = 
            if isOk (maze' position')
              then s { sPosition = position' }
            else if maze' position' == Box && isOk (maze' position'')
              then s { sPosition = position', sBoxes = movedBoxes }
            else
              s
      in newS { sDirection = direction }

    isOk :: Tile -> Bool
    isOk Ground = True
    isOk Storage = True
    isOk _ = False
handleEvent _ c = c

isWon :: State -> Bool
isWon (State _ _ boxes) = allList (mapList isOnStorage boxes)

isOnStorage :: Coord -> Bool
isOnStorage coord = maze coord == Storage

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
pictureOfMaze = traverseCoords drawTileAt

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

winScreen :: Picture
winScreen = scaled 3 3 (lettering "Haskell!")

drawState :: State -> Picture
drawState state@(State position direction boxes) =
  (if isWon state then winScreen else blank) &
  atCoord position (player direction) &
  pictureOfBoxes boxes &
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

-- Activities with undo

data WithUndo a = WithUndo a (List a)

withUndo :: Eq a => Activity a -> Activity (WithUndo a)
withUndo (Activity state0 handle draw) =
  Activity state0' handle' draw'
  where
    state0' = WithUndo state0 Empty

    draw' (WithUndo s _) = draw s

    handle' (KeyPress key) state@(WithUndo _ stack) | key == "U" =
      case stack of
        Empty -> state
        (Entry s' stack') -> WithUndo s' stack'
    handle' e state@(WithUndo s stack) =
      let newS = handle e s
      in if newS == s then state else WithUndo newS (Entry s stack)

-- The main function

main :: IO ()
main = runActivity (resetable (withStartScreen (withUndo sokoban)))
