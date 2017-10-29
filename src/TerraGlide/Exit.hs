module TerraGlide.Exit
    ( onExit
    ) where

import           Scene            (Viewer)
import           TerraGlide.State (State)

onExit :: Viewer -> State -> IO ()
onExit _ _ = return ()
