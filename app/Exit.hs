module Exit
    ( onExit
    ) where

import           Scene (Viewer)
import           State (State)

onExit :: Viewer -> State -> IO ()
onExit _ _ = return ()
