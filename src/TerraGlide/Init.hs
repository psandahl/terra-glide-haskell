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
import qualified TerraGlide.GUI              as GUI
import           TerraGlide.Options          (Options (..))
import           TerraGlide.State            (State (..))
import qualified TerraGlide.Terrain          as Terrain
import qualified TerraGlide.Water            as Water

-- | Make the 'Configuration' for Terra Glide.
configuration :: Options -> Configuration
configuration options =
    Configuration
        { caption = "Terra Glide"
        , glVersionMajor = 3
        , glVersionMinor = 3
        , displayMode = toDisplayMode <| fullScreen options
        , globalSettings =
            [ SetClearColor (114 / 255) (171 / 255) (245 / 255) 0
            , Enable DepthTest
            , SetDepthFunc Less
            , Enable CullFace
            , SetCullFace Back
            ]
        , initialScene =
            Scene
                { sceneSettings =
                    [ Clear [ColorBufferBit, DepthBufferBit]
                    ]
                , firstRendering = Nothing
                }
        , debugContext = debug options
        }

-- | Perform the initialization once gl-scene has started.
onInit :: Bool -> Viewer -> IO (Maybe State)
onInit debug' viewer = do
    let environment = Environment.init
    eRefraction <- framebufferFromRequest viewer (FramebufferRequest 1024 768)
    eReflection <- framebufferFromRequest viewer (FramebufferRequest 1024 768)
    eTerrain <- Terrain.init viewer environment (V3 0 3 10)
    eWater <- Water.init viewer
    eGUI <- GUI.init viewer
    case (eTerrain, eWater, eRefraction, eReflection, eGUI) of
        ( Right terrain, Right water, Right refractionFramebuffer,
          Right reflectionFramebuffer, Right gui) -> do

            subscribeKeyboard viewer
            subscribeMouseButton viewer
            subscribeCursurPos viewer

            return <|
                Just State
                    { _debug = debug'
                    , _environment = environment
                    , _mainCamera = initMainCamera
                    , _mainCameraNavigation = CameraNavigation.init
                    , _terrain = terrain
                    , _water = water
                    , _refractionFramebuffer = refractionFramebuffer
                    , _reflectionFramebuffer = reflectionFramebuffer
                    , _gui = gui
                    }

        (Left err, _, _, _, _) -> do
            sceneLog viewer <| toLogStr err
            return Nothing

        (_, Left err, _, _, _) -> do
            sceneLog viewer <| toLogStr err
            return Nothing

        (_, _, Left err, _, _) -> do
            sceneLog viewer <| toLogStr err
            return Nothing

        (_, _, _, Left err, _) -> do
            sceneLog viewer <| toLogStr err
            return Nothing

        (_, _, _, _, Left err) -> do
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
