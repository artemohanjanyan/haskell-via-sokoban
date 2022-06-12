module Drawing where

import Core
import List
import State

type Picture = String

drawTile :: Tile -> String
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = "  "

atCoord :: Coord -> Picture -> Picture
atCoord (C x _) pic = pic ++ if x == 10 then "\n" else ""

drawState :: State -> Picture
drawState state = unlines $ reverse $ lines $
  traverseCoords $ \c -> atCoord c $
    if c == sPosition state
      then player (sDirection state)
    else if containsList c (sBoxes state)
      then box
    else
      drawTile (noBoxMaze (lMaze (sCurrentLevel state)) c)

wall, ground, storage, box :: Picture
wall = ['\129704', ' '] -- 🪨
ground = "🌱"
storage = "❓"
box = "📦"

player :: Direction -> Picture
player R = "😏"
player L = ['\129488'] -- 🧐
player U = ['\128580'] -- 🙄
player D = "😌"

winScreen :: Picture
winScreen = "Haskell!"

startScreen :: Picture
startScreen = "Sokoban!"
