{-# LANGUAGE TemplateHaskell #-}
module TerraGlide.CameraNavigation
    ( CameraNavigation
    , init
    , animate
    , forward
    , backward
    , turnLeft
    , turnRight
    ) where

import           Control.Lens (makeLenses)
import           Flow         ((<|))
import           Prelude      hiding (init)
import           Scene        (GLfloat)
import           Scene.Camera (Camera)
import qualified Scene.Camera as Camera

-- | Record containing information how a camera shall navigate.
data CameraNavigation = CameraNavigation
    { _forward   :: !Bool
    , _backward  :: !Bool
    , _turnLeft  :: !Bool
    , _turnRight :: !Bool
    } deriving Show

makeLenses ''CameraNavigation

-- | Initial navigation record.
init :: CameraNavigation
init =
    CameraNavigation
        { _forward = False
        , _backward = False
        , _turnLeft = False
        , _turnRight = False
        }

animate :: GLfloat -> CameraNavigation -> Camera -> Camera
animate duration navigation camera =
    animateForward duration navigation <|
        animateBackward duration navigation camera

animateForward :: GLfloat -> CameraNavigation -> Camera -> Camera
animateForward duration navigation camera =
    if _forward navigation
        then Camera.forward duration camera
        else camera

animateBackward :: GLfloat -> CameraNavigation -> Camera -> Camera
animateBackward duration navigation camera =
    if _backward navigation
        then Camera.backward duration camera
        else camera
