module Activity where

import List

data Activity event picture state = Activity
  { aStart :: state
  , aHandle :: event -> state -> state
  , aDraw :: state -> picture
  }

resettable
  :: Eq event
  => event
  -> Activity event picture state
  -> Activity event picture state
resettable resetEvent activity = activity { aHandle = handle' }
  where
    handle' event _ | event == resetEvent = aStart activity
    handle' event state = aHandle activity event state

data StartScreenState state = StartScreen | Running state

withStartScreen
  :: Eq event
  => event
  -> picture
  -> Activity event picture state
  -> Activity event picture (StartScreenState state)
withStartScreen skipStartScreenEvent startScreen activity
  = Activity start' handle' draw'
  where
    start' = StartScreen

    handle' e StartScreen
      | e == skipStartScreenEvent = Running (aStart activity)
      | otherwise                 = StartScreen
    handle' e (Running s)         = Running (aHandle activity e s)

    draw' StartScreen = startScreen
    draw' (Running s) = aDraw activity s

data WithUndo state = WithUndo
  { wuState :: state
  , wuHistory :: List state
  }

withUndo
  :: (Eq event, Eq state)
  => event
  -> Activity event picture state
  -> Activity event picture (WithUndo state)
withUndo undoEvent activity =
  Activity start' handle' draw'
  where
    start' = WithUndo (aStart activity) Empty

    draw' (WithUndo s _) = aDraw activity s

    handle' event state@(WithUndo _ stack) | event == undoEvent =
      case stack of
        Empty -> state
        (Entry s' stack') -> WithUndo s' stack'
    handle' e state@(WithUndo s stack) =
      let newS = aHandle activity e s
      in if newS == s then state else WithUndo newS (Entry s stack)
