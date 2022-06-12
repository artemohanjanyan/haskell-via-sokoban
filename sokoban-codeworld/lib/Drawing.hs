{-# LANGUAGE OverloadedStrings #-}

module Drawing where

import CodeWorld

import List
import Core
import Pictures
import State

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

pictureOfMaze :: Maze -> Picture
pictureOfMaze maze = traverseCoords (drawTileAt maze)

drawTileAt :: Maze -> Coord -> Picture
drawTileAt maze c = atCoord c (drawTile (noBoxMaze maze c))

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes cs = combine (mapList (\c -> atCoord c (drawTile Box)) cs)

winScreen :: Picture
winScreen = scaled 3 3 (lettering "Haskell!")

drawState :: State -> Picture
drawState state =
  atCoord (sPosition state) (player (sDirection state)) &
  pictureOfBoxes (sBoxes state) &
  pictureOfMaze (lMaze $ sCurrentLevel state)
