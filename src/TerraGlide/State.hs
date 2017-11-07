{-# LANGUAGE TemplateHaskell #-}
module TerraGlide.State
    ( State (..) -- TODO: As State is a Lens, change the export.
    , mainCamera
    , mainCameraNavigation
    , dummyMesh
    , dummyProgram
    ) where

import           Control.Lens                (makeLenses)
import           Scene                       (Mesh, Program)
import           Scene.Camera                (Camera)
import           TerraGlide.CameraNavigation (CameraNavigation)

-- | State record for Terra Glide.
data State = State
    { _mainCamera           :: !Camera
    , _mainCameraNavigation :: !CameraNavigation
    , _dummyMesh            :: !Mesh
    , _dummyProgram         :: !Program
    } deriving Show

makeLenses ''State
