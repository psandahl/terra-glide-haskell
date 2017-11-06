module TerraGlide.CameraNavigation
    ( CameraNavigation (..)
    , init
    , animate
    ) where


import           Prelude      hiding (init)
import           Scene        (GLfloat)
import           Scene.Camera (Camera)
import qualified Scene.Camera as Camera

-- | Record containing information how a camera shall navigate.
data CameraNavigation = CameraNavigation
    { forward :: !Bool
    } deriving Show

-- | Initial navigation record.
init :: CameraNavigation
init =
    CameraNavigation
        { forward = False
        }

animate :: GLfloat -> CameraNavigation -> Camera -> Camera
animate = animateForward

animateForward :: GLfloat -> CameraNavigation -> Camera -> Camera
animateForward duration navigation camera =
    if forward navigation
        then Camera.forward duration camera
        else camera
