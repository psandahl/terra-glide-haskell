{-# LANGUAGE OverloadedStrings #-}
module TerraGlide.Event
    ( onEvent
    ) where

import           Flow             ((<|))
import           Graphics.GL      (GLfloat)
import           Linear           (M44, V3 (..), (!*!))
import           Scene
import qualified Scene.Camera     as Camera
import           Scene.Math       (Angle (..), mkPerspectiveMatrix,
                                   mkViewMatrix)
import           TerraGlide.State (State (..))

onEvent :: Viewer -> Event -> Maybe State -> IO (Maybe State)

onEvent viewer (Frame _ viewport) (Just state) = do
    let perspMatrix = mkPerspectiveMatrix (Degrees 45) viewport 1 100
        --viewMatrix = Camera.matrix <| mainCamera state
        viewMatrix = mkViewMatrix (V3 0 3 10) (V3 0 0 0) (V3 0 1 0)
        mvpMatrix = perspMatrix !*! viewMatrix :: M44 GLfloat

    setScene viewer <|
        Scene
            { sceneSettings =
                [ Clear [ColorBufferBit, DepthBufferBit]
                ]
            , sceneEntities =
                [ Entity
                    { entitySettings = []
                    , entityProgram = dummyProgram state
                    , entityMesh = dummyMesh state
                    , entityUniforms =
                        [ UniformValue "mvpMatrix" mvpMatrix
                        ]
                    , entityTextures = []
                    }
                ]
            }
    return (Just state)

onEvent viewer CloseRequest (Just state) = do
    close viewer
    return (Just state)

-- A previous handler, init or event, have returned Nothing due to problems.
onEvent viewer _ Nothing = do
    close viewer
    return Nothing
