{-# LANGUAGE OverloadedStrings #-}
module TerraGlide.Init
    ( configuration
    , onInit
    ) where

import           Flow                        ((<|))
import           Linear                      (V3 (..))
import           Scene
import           Scene.Camera                (Camera, Direction (..), mkCamera)
import           Scene.Math                  (Angle (..))
import qualified TerraGlide.CameraNavigation as CameraNavigation
import qualified TerraGlide.Environment      as Environment
import           TerraGlide.Options          (Options (..))
import           TerraGlide.State            (State (..))
import           TerraGlide.Terrain          as Terrain

-- | Make the 'Configuration' for Terra Glide.
configuration :: Options -> Configuration
configuration options =
    Configuration
        { caption = "Terra Glide"
        , glVersionMajor = 3
        , glVersionMinor = 3
        , displayMode = toDisplayMode <| fullScreen options
        , globalSettings =
            [ SetClearColor 0 0 1 0
            , Enable DepthTest
            , SetDepthFunc Less
            ]
        , initialSceneGraph =
            SceneGraph
                { sceneGraphSettings =
                    [ Clear [ColorBufferBit, DepthBufferBit]
                    ]
                , sceneGraphEntities = []
                }
        , debugContext = debug options
        }

-- | Perform the initialization once gl-scene has started.
onInit :: Bool -> Viewer -> IO (Maybe State)
onInit debug' viewer = do
    let environment' = Environment.init
    eTerrain <- Terrain.init viewer environment' (V3 0 3 10)
    case eTerrain of
        Right terrain' -> do

            subscribeKeyboard viewer
            subscribeMouseButton viewer
            subscribeCursurPos viewer

            return <|
                Just State
                    { _debug = debug'
                    , _environment = environment'
                    , _mainCamera = initMainCamera
                    , _mainCameraNavigation = CameraNavigation.init
                    , _terrain = terrain'
                    }
        Left err -> do
            sceneLog viewer <| toLogStr err
            return Nothing

initMainCamera :: Camera
initMainCamera =
    let viewDirection = Direction { _heading = Degrees 0, _elevation = Degrees (-15) }
        moveDirection = Direction { _heading = Degrees 0, _elevation = Degrees 0 }
    in mkCamera (V3 0 500 0) viewDirection moveDirection

toDisplayMode :: Bool -> DisplayMode
toDisplayMode True  = FullScreen
toDisplayMode False = Windowed 1024 768
