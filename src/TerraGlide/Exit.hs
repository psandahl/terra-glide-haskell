module TerraGlide.Exit
    ( onExit
    ) where

import           Scene            (Viewer)
import           TerraGlide.State (State)

onExit :: Viewer -> Maybe State -> IO ()
onExit _ _ = return ()
