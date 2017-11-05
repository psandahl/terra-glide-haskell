{-# LANGUAGE OverloadedStrings #-}
module TerraGlide.Init
    ( configuration
    , onInit
    ) where

import           Data.Vector.Storable             (Vector)
import qualified Data.Vector.Storable             as Vector
import           Flow                             ((<|))
import           Graphics.GL                      (GLuint)
import           Linear                           (V3 (..))
import           Scene
import           Scene.Camera                     (Camera, Direction (..),
                                                   mkCamera)
import qualified Scene.GL.Attribute.VertexWithPos as WithPos
import           Scene.Math                       (Angle (..))
import           TerraGlide.Options               (Options (..))
import           TerraGlide.State                 (State (..))

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
        , initialScene =
            Scene
                { sceneSettings =
                    [ Clear [ColorBufferBit, DepthBufferBit]
                    ]
                , sceneEntities = []
                }
        , debugContext = debug options
        }

-- | Perform the initialization once gl-scene has started.
onInit :: Viewer -> IO (Maybe State)
onInit viewer = do
    dummyMesh' <- loadDummyMesh viewer
    dummyProgram' <- loadDummyProgram viewer
    case (dummyMesh', dummyProgram') of
        (Right m, Right p) -> do
            subscribeKeyboard viewer
            return <|
                Just State
                    { mainCamera = initMainCamera
                    , dummyMesh = m
                    , dummyProgram = p
                    }
        (_, Left err) -> do
            sceneLog viewer <| toLogStr err
            return Nothing
        _ -> do
            sceneLog viewer <| toLogStr ("onInit: Cannot load resources" :: String)
            return Nothing

initMainCamera :: Camera
initMainCamera =
    let viewDirection = Direction { heading = Degrees 180, elevation = Degrees (-15) }
        moveDirection = Direction { heading = Degrees 180, elevation = Degrees 0 }
    in mkCamera (V3 0 3 10) viewDirection moveDirection

toDisplayMode :: Bool -> DisplayMode
toDisplayMode True  = FullScreen
toDisplayMode False = Windowed 1024 768

-- Dummy stuff while developing camera functionality.

loadDummyMesh :: Viewer -> IO (Either String Mesh)
loadDummyMesh viewer =
    meshFromRequest viewer <| MeshRequest dummyVertices dummyIndices Triangles

loadDummyProgram :: Viewer -> IO (Either String Program)
loadDummyProgram viewer =
    programFromFiles viewer <|
        ProgramRequest [ (Vertex, "resources/shader/dummy.vert")
                       , (Fragment, "resources/shader/dummy.frag")
                       ] ["mvpMatrix"]

dummyVertices :: Vector WithPos.Vertex
dummyVertices =
    Vector.fromList
        [ WithPos.Vertex { WithPos.position = V3 (-1) 0 (-1) }
        , WithPos.Vertex { WithPos.position = V3 1 0 (-1) }
        , WithPos.Vertex { WithPos.position = V3 (-1) 0 1 }
        , WithPos.Vertex { WithPos.position = V3 1 0 1 }
        ]

dummyIndices :: Vector GLuint
dummyIndices =
    Vector.fromList
        [ 1, 0, 2, 1, 2, 3
        ]
