module TerraGlide.Event
    ( onEvent
    ) where

import           Scene            (Event (..), Viewer, close)
import           TerraGlide.State (State)

onEvent :: Viewer -> Event -> Maybe State -> IO (Maybe State)

onEvent _viewer (Frame _ _) (Just state) =
    return (Just state)

onEvent viewer CloseRequest (Just state) = do
    close viewer
    return (Just state)

-- A previous handler, init or event, have returned Nothing due to problems.
onEvent viewer _ Nothing = do
    close viewer
    return Nothing
