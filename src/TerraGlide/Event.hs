module TerraGlide.Event
    ( onEvent
    ) where

import           Scene            (Event (..), Viewer, close)
import           TerraGlide.State (State)

onEvent :: Viewer -> Event -> State -> IO State

onEvent _viewer (Frame _ _) state =
    return state

onEvent viewer CloseRequest state = do
    close viewer
    return state
