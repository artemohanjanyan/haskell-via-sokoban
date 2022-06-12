module Sokoban where

import Core
import List
import Activity
import State (State(..))
import qualified State as State
import qualified Mazes as Mazes

data LevelSelect state = LevelSelect
  { lsLevels :: List Level
  , lsCurrentLevelI :: Int
  , lsState :: state
  }

withLevelSelect
  :: Eq event
  => List Level
  -> (Level -> state)
  -> (state -> Bool)
  -> event
  -> event
  -> picture
  -> (state -> Activity event picture state)
  -> Activity event picture (LevelSelect state)
withLevelSelect
  levels
  initState
  isLevelComplete
  prevLevelEvent
  nextLevelEvent
  winScreenPicture
  activity'
  = Activity start' handle' draw'
  where
    firstLevelI = 1

    initLevelState i = LevelSelect
      { lsLevels = levels
      , lsCurrentLevelI = i
      , lsState = initState (nth levels i)
      }

    start' = initLevelState firstLevelI
    activity state = activity' $ lsState $ changeLevel state 0

    changeLevel state d =
      let i' = lsCurrentLevelI state + d
          levelN = listLength $ lsLevels state
      in initLevelState $ min (max i' firstLevelI) levelN

    handle' event state
      | event == prevLevelEvent = changeLevel state (-1)
      | event == nextLevelEvent = changeLevel state 1
      | isLevelComplete $ lsState state = state
      | otherwise =
        let lsState' = aHandle (activity state) event $ lsState state
        in if isLevelComplete lsState' &&
            lsCurrentLevelI state < listLength levels
          then changeLevel state 1
          else state { lsState = lsState' }

    draw' state
      | isLevelComplete $ lsState state = winScreenPicture
      | otherwise = aDraw (activity state) $ lsState state

data Config event picture = Config
  { cEventUp :: event
  , cEventDown :: event
  , cEventLeft :: event
  , cEventRight :: event
  , cDrawState :: State -> picture
  , cEventUndo :: event
  , cEventReset :: event
  , cEventStart :: event
  , cStartScreen :: picture
  , cEventPrevLevel :: event
  , cEventNextLevel :: event
  , cWinScreen :: picture
  }

sokoban
  :: Eq event
  => Config event picture
  -> State
  -> Activity event picture State
sokoban config aStart' = Activity
  { aStart = aStart'
  , aHandle = handle
  , aDraw = cDrawState config
  }
  where
    handle event state
      | event == cEventUp    config = f U
      | event == cEventDown  config = f D
      | event == cEventLeft  config = f L
      | event == cEventRight config = f R
      | otherwise = state
      where
        f direction = State.movePlayer direction state

type State' = StartScreenState (LevelSelect (WithUndo State))

sokoban'
  :: Eq event
  => Config event picture
  -> Activity event picture State'
sokoban' config =
  withStartScreen (cEventStart config) (cStartScreen config) $
  withLevelSelect
    Mazes.levels
    (flip WithUndo Empty . State.initialState)
    (State.isLevelComplete . wuState)
    (cEventPrevLevel config)
    (cEventNextLevel config)
    (cWinScreen config) $ \aStart' ->
      resettable (cEventReset config) $
      withUndo (cEventUndo config) $
      sokoban config $ wuState aStart'
