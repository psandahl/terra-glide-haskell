module TerraGlide.State
    ( State (..)
    ) where

import           Scene        (Mesh, Program)
import           Scene.Camera (Camera)

-- | State record for Terra Glide.
data State = State
    { mainCamera   :: !Camera
    , dummyMesh    :: !Mesh
    , dummyProgram :: !Program
    } deriving Show
