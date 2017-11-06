module TerraGlide.CameraNavigation
    ( CameraNavigation (..)
    , init
    , animate
    ) where

import           Flow         ((<|))
import           Prelude      hiding (init)
import           Scene        (GLfloat)
import           Scene.Camera (Camera)
import qualified Scene.Camera as Camera

-- | Record containing information how a camera shall navigate.
data CameraNavigation = CameraNavigation
    { forward  :: !Bool
    , backward :: !Bool
    } deriving Show

-- | Initial navigation record.
init :: CameraNavigation
init =
    CameraNavigation
        { forward = False
        , backward = False
        }

animate :: GLfloat -> CameraNavigation -> Camera -> Camera
animate duration navigation camera =
    animateForward duration navigation <|
        animateBackward duration navigation camera

animateForward :: GLfloat -> CameraNavigation -> Camera -> Camera
animateForward duration navigation camera =
    if forward navigation
        then Camera.forward duration camera
        else camera

animateBackward :: GLfloat -> CameraNavigation -> Camera -> Camera
animateBackward duration navigation camera =
    if backward navigation
        then Camera.backward duration camera
        else camera
