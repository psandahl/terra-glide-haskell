{-# LANGUAGE TemplateHaskell #-}
module TerraGlide.CameraNavigation
    ( CameraNavigation
    , init
    , animate
    , changeView
    , forward
    , backward
    , up
    , down
    , turnLeft
    , turnRight
    , lastCursorPos
    ) where

import           Control.Lens (makeLenses, (^.))
import           Flow         ((<|))
import           Linear       (V2 (..), _x, _y)
import           Prelude      hiding (init)
import           Scene        (GLfloat)
import           Scene.Camera (Camera)
import qualified Scene.Camera as Camera
import           Scene.Math   (Angle (..), mulAngle)

-- | Record containing information how a camera shall navigate.
data CameraNavigation = CameraNavigation
    { _forward       :: !Bool
    , _backward      :: !Bool
    , _up            :: !Bool
    , _down          :: !Bool
    , _turnLeft      :: !Bool
    , _turnRight     :: !Bool
    , _lastCursorPos :: !(Maybe (V2 Double))
    } deriving Show

makeLenses ''CameraNavigation

-- | Initial navigation record.
init :: CameraNavigation
init =
    CameraNavigation
        { _forward = False
        , _backward = False
        , _up = False
        , _down = False
        , _turnLeft = False
        , _turnRight = False
        , _lastCursorPos = Nothing
        }

-- | Make the 'Camera' animate according to the duration and the 'CameraNavigation'
-- settings.
animate :: GLfloat -> CameraNavigation -> Camera -> Camera
animate duration navigation camera =
    animateForward duration navigation <|
        animateBackward duration navigation <|
        animateUp duration navigation <|
        animateDown duration navigation <|
        animateTurnRight duration navigation <|
        animateTurnLeft duration navigation camera

-- | Change the view of the 'Camera' with help of the cursor positions.
changeView :: V2 Double -> V2 Double -> Camera -> Camera
changeView newCursorPos oldCursorPos camera =
    let xDelta = realToFrac <| newCursorPos ^. _x - oldCursorPos ^. _x
        yDelta = realToFrac <| newCursorPos ^. _y - oldCursorPos ^. _y
    in changeHeading xDelta <| changeElevation yDelta camera

changeHeading :: GLfloat -> Camera -> Camera
changeHeading delta camera
    | delta < 0 = Camera.viewLeft (Degrees <| abs delta) camera
    | otherwise = Camera.viewRight (Degrees delta) camera

changeElevation :: GLfloat -> Camera -> Camera
changeElevation delta camera
    | delta < 0 = Camera.viewDown (Degrees <| abs delta) camera
    | otherwise = Camera.viewUp (Degrees delta) camera

animateForward :: GLfloat -> CameraNavigation -> Camera -> Camera
animateForward duration navigation camera =
    if navigation ^. forward
        then Camera.forward duration camera
        else camera

animateBackward :: GLfloat -> CameraNavigation -> Camera -> Camera
animateBackward duration navigation camera =
    if navigation ^. backward
        then Camera.backward duration camera
        else camera

animateUp :: GLfloat -> CameraNavigation -> Camera -> Camera
animateUp duration navigation camera =
    if navigation ^. up
        then Camera.up duration camera
        else camera

animateDown :: GLfloat -> CameraNavigation -> Camera -> Camera
animateDown duration navigation camera =
    if navigation ^. down
        then Camera.down duration camera
        else camera

animateTurnLeft :: GLfloat -> CameraNavigation -> Camera -> Camera
animateTurnLeft duration navigation camera =
    if navigation ^. turnLeft
        then Camera.turnLeft (mulAngle (Degrees 180) duration) camera
        else camera

animateTurnRight :: GLfloat -> CameraNavigation -> Camera -> Camera
animateTurnRight duration navigation camera =
    if navigation ^. turnRight
        then Camera.turnRight (mulAngle (Degrees 180) duration) camera
        else camera
