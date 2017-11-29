{-# LANGUAGE TemplateHaskell #-}
module TerraGlide.State
    ( State (..) -- TODO: As State is a Lens, change the export.
    , debug
    , environment
    , mainCamera
    , mainCameraNavigation
    , terrain
    , water
    , refractionFramebuffer
    , gui
    ) where

import           Control.Lens                (makeLenses)
import           Scene                       (Framebuffer)
import           Scene.Camera                (Camera)
import           TerraGlide.CameraNavigation (CameraNavigation)
import           TerraGlide.Environment      (Environment)
import           TerraGlide.GUI              (GUI)
import           TerraGlide.Terrain          (Terrain)
import           TerraGlide.Water            (Water)

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
    , _water                 :: !Water
    -- ^ The water.
    , _refractionFramebuffer :: !Framebuffer
    -- ^ Framebuffer for the water refraction texture.
    , _gui                   :: !GUI
    } deriving Show

makeLenses ''State
