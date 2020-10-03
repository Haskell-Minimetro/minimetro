{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module ActivityOfEnhancements where
import CodeWorld

type ActivityOf world
  = world
    -> (Event -> world -> world)
    -> (world -> Picture)
    -> IO ()
    
-- | Interaction state for 'world' with start screen.
data WithStartScreen world
  = StartScreen  -- ˆ Start screen.
    | GameOn world -- ˆ Game is on with 'world' state.
-- | Add start screen to 'activityOf'.
withStartScreen
  :: forall world. ActivityOf (WithStartScreen world)
  -> ActivityOf world
  
startScreen :: Picture
startScreen = lettering "Mini Metro\n[press SPACE to start]" <> background
  where
    background = colored (lighter 0.5 brown) (solidRectangle 100 100)

-- | Add start screen to 'activityOf'.
withStartScreen oldActivity startingWorld userHandle userDraw = oldActivity StartScreen newHandle newDraw
  where
    newHandle (KeyPress " ") StartScreen  = GameOn startingWorld
    newHandle event (GameOn state)      = GameOn (userHandle event state)
    newHandle _ state                   = state
    
    newDraw StartScreen   = startScreen
    newDraw (GameOn s)    = userDraw s
-- | Make 'activityOf' resettable on Esc.
withReset :: ActivityOf world -> ActivityOf world
withReset originalActivityOf userWorld userHandler =
  originalActivityOf userWorld (updateHandler userWorld userHandler) 

-- | Update handler function that will put initial state in place
-- If eas is pressed
updateHandler :: forall world.
  world -> (Event -> world -> world) -> (Event -> world -> world)
updateHandler initial userHandler = updatedHandler 
  where
    updatedHandler :: Event -> world -> world
    updatedHandler (KeyPress "Esc") _ = initial
    updatedHandler event state = userHandler event state

    