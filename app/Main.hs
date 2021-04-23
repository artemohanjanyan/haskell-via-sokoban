{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

import Core
import Activity
import State
import Drawing

handleEvent :: Event -> Maybe State -> Maybe State
handleEvent _ Nothing = Nothing
handleEvent (KeyPress key) (Just state)
  | key == "Right" = f R
  | key == "Up"    = f U
  | key == "Left"  = f L
  | key == "Down"  = f D
  | key == "N"     = nextLevel state
  where
    f direction =
      let newState = movePlayer direction state in
      if isLevelComplete newState
        then nextLevel newState
        else (Just newState)
handleEvent _ s = s

firstLevelState :: Maybe State
firstLevelState = Just (initialState 1)

drawMaybeState :: Maybe State -> Picture
drawMaybeState Nothing = winScreen
drawMaybeState (Just state) = drawState state

sokoban :: Activity (Maybe State)
sokoban = Activity firstLevelState handleEvent drawMaybeState

main :: IO ()
main = runActivity (resetable (withStartScreen (withUndo sokoban)))
