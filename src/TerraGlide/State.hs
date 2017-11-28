{-# LANGUAGE TemplateHaskell #-}
module TerraGlide.State
    ( State (..) -- TODO: As State is a Lens, change the export.
    , debug
    , environment
    , mainCamera
    , mainCameraNavigation
    , terrain
    , rearMirrorFramebuffer
    ) where

import           Control.Lens                (makeLenses)
import           Scene                       (Framebuffer)
import           Scene.Camera                (Camera)
import           TerraGlide.CameraNavigation (CameraNavigation)
import           TerraGlide.Environment      (Environment)
import           TerraGlide.Terrain          (Terrain)

-- | State record for Terra Glide.
data State = State
    { _debug                 :: !Bool
    -- ^ Flag telling if the user has requested debug traceing.
    , _environment           :: !Environment
    -- ^ Environment configuration.
    , _mainCamera            :: !Camera
    -- ^ The main camera.
    , _mainCameraNavigation  :: !CameraNavigation
    -- ^ Navigation flags for the main camera.
    , _terrain               :: !Terrain
    -- ^ The terrain manager.
    , _rearMirrorFramebuffer :: !Framebuffer
    -- ^ Framebuffer for the rear mirror view.
    } deriving Show

makeLenses ''State
