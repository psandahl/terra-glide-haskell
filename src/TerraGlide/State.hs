{-# LANGUAGE TemplateHaskell #-}
module TerraGlide.State
    ( State (..) -- TODO: As State is a Lens, change the export.
    , debug
    , mainCamera
    , mainCameraNavigation
    , terrain
    ) where

import           Control.Lens                (makeLenses)
import           Scene.Camera                (Camera)
import           TerraGlide.CameraNavigation (CameraNavigation)
import           TerraGlide.Terrain          (Terrain)

-- | State record for Terra Glide.
data State = State
    { _debug                :: !Bool
    , _mainCamera           :: !Camera
    , _mainCameraNavigation :: !CameraNavigation
    , _terrain              :: !Terrain
    } deriving Show

makeLenses ''State
