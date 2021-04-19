{-# LANGUAGE OverloadedStrings #-}

module Activity where

import CodeWorld

import List

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

withStartScreen :: Activity s -> Activity (SSState s)
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
