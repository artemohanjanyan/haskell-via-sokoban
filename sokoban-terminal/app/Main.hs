module Main where

import Data.Function (fix)
import System.IO (hReady, hSetBuffering, stdin, stdout, BufferMode (NoBuffering))

import Activity
import Drawing
import Sokoban

type Event = String

config :: Config Event Picture
config = Config
  { cEventUp = "\ESC[A"
  , cEventDown = "\ESC[B"
  , cEventLeft = "\ESC[D"
  , cEventRight = "\ESC[C"
  , cDrawState = drawState
  , cEventUndo = "u"
  , cEventReset = "\ESC"
  , cEventStart = " "
  , cStartScreen = startScreen
  , cEventPrevLevel = "p"
  , cEventNextLevel = "n"
  , cWinScreen = winScreen
  }

getAllInput :: IO String
getAllInput = do
  c <- getChar
  isReady <- hReady stdin
  if not isReady
    then pure [c]
    else (c:) <$> getAllInput

runActivity :: Activity Event Picture state -> IO ()
runActivity activity = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  flip fix (aStart activity) $ \rec state -> do
    putStr "\ESCc"
    putStr (aDraw activity state)
    event <- getAllInput
    let newState = aHandle activity event state
    rec newState

main :: IO ()
main = runActivity $ sokoban' config
