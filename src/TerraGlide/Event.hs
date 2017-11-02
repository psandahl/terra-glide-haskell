{-# LANGUAGE OverloadedStrings #-}
module TerraGlide.Event
    ( onEvent
    ) where

import           Flow             ((<|))
import           Linear           ((!*!))
import           Scene
import qualified Scene.Camera     as Camera
import           Scene.Math       (Angle (..), mkPerspectiveMatrix)
import           TerraGlide.State (State (..))

onEvent :: Viewer -> Event -> Maybe State -> IO (Maybe State)

onEvent viewer (Frame _ viewport) (Just state) = do
    let perspMatrix = mkPerspectiveMatrix (Degrees 45) viewport 1 100
        viewMatrix = Camera.matrix <| mainCamera state
        mvpMatrix = perspMatrix !*! viewMatrix

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

onEvent viewer (KeyStroke key keyState _) s@(Just _state) = do
    sceneLog viewer <| toLogStr ("KeyStroke: " ++ show key ++ ", " ++ show keyState)
    return s

onEvent viewer CloseRequest (Just state) = do
    close viewer
    return (Just state)

-- A previous handler, init or event, have returned Nothing due to problems.
onEvent viewer _ Nothing = do
    close viewer
    return Nothing
