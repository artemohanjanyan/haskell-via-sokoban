module Activity where

import List

data Activity event picture state = Activity
  { start :: state
  , handle :: event -> state -> state
  , draw :: state -> picture
  }

resettable
  :: Eq event
  => event
  -> Activity event picture state
  -> Activity event picture state
resettable resetEvent activity = activity { handle = handle' }
  where
    handle' event _ | event == resetEvent = start activity
    handle' event state = handle activity event state

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
      | e == skipStartScreenEvent = Running (start activity)
      | otherwise                 = StartScreen
    handle' e (Running s)         = Running (handle activity e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw activity s

data WithUndo state = WithUndo state (List state)

withUndo
  :: (Eq event, Eq state)
  => event
  -> Activity event picture state
  -> Activity event picture (WithUndo state)
withUndo undoEvent activity =
  Activity start' handle' draw'
  where
    start' = WithUndo (start activity) Empty

    draw' (WithUndo s _) = draw activity s

    handle' event state@(WithUndo _ stack) | event == undoEvent =
      case stack of
        Empty -> state
        (Entry s' stack') -> WithUndo s' stack'
    handle' e state@(WithUndo s stack) =
      let newS = handle activity e s
      in if newS == s then state else WithUndo newS (Entry s stack)
