{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

import Core
import Activity
import State
import Drawing

handleEvent :: Event -> State -> State
handleEvent _ state | isWon state = state
handleEvent (KeyPress key) state
  | key == "Right" = f R
  | key == "Up"    = f U
  | key == "Left"  = f L
  | key == "Down"  = f D
  where
    f = flip movePlayer state
handleEvent _ c = c

sokoban :: Activity State
sokoban = Activity initialState handleEvent drawState

main :: IO ()
main = runActivity (resetable (withStartScreen (withUndo sokoban)))
