module Main where

import Data.Function (fix)
import System.IO (hReady, hSetBuffering, stdin, stdout, BufferMode (NoBuffering))

import Core
import Activity
import State
import Drawing

type Event = String

handleEvent :: Event -> Maybe State -> Maybe State
handleEvent _ Nothing = Nothing
handleEvent event (Just state)
  | event == "\ESC[C" = f R
  | event == "\ESC[A" = f U
  | event == "\ESC[D" = f L
  | event == "\ESC[B" = f D
  | event == "n"      = nextLevel state
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

sokoban :: Activity Event Picture (Maybe State)
sokoban = Activity firstLevelState handleEvent drawMaybeState

getAllInput :: IO String
getAllInput = do
  c <- getChar
  isReady <- hReady stdin
  if not isReady
    then pure [c]
    else (c:) <$> getAllInput

runActivity :: Activity Event Picture state -> IO ()
runActivity (Activity state handle draw) = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  flip fix state $ \rec state -> do
    putStr "\ESCc"
    putStr (draw state)
    event <- getAllInput
    let newState = handle event state
    rec newState

main :: IO ()
main = runActivity sokoban'
  where
    sokoban' =
      resettable "\ESC" $
      withStartScreen " " startScreen $
      withUndo "u" $
      sokoban
