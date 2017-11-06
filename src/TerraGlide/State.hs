module TerraGlide.State
    ( State (..)
    ) where

import           Scene                       (Mesh, Program)
import           Scene.Camera                (Camera)
import           TerraGlide.CameraNavigation (CameraNavigation)

-- | State record for Terra Glide.
data State = State
    { mainCamera           :: !Camera
    , mainCameraNavigation :: !CameraNavigation
    , dummyMesh            :: !Mesh
    , dummyProgram         :: !Program
    } deriving Show
