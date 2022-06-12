{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

import Activity
import Drawing
import Pictures
import Sokoban

config :: Config Event Picture
config = Config
  { cEventUp = KeyPress "Up"
  , cEventDown = KeyPress "Down"
  , cEventLeft = KeyPress "Left"
  , cEventRight = KeyPress "Right"
  , cDrawState = drawState
  , cEventUndo = KeyPress "U"
  , cEventReset = KeyPress "Esc"
  , cEventStart = KeyPress " "
  , cStartScreen = startScreen
  , cEventPrevLevel = KeyPress "P"
  , cEventNextLevel = KeyPress "N"
  , cWinScreen = winScreen
  }

runActivity :: Activity Event Picture state -> IO ()
runActivity (Activity start' handle' draw') =
  activityOf start' handle' draw'

main :: IO ()
main = runActivity $ sokoban' config
