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

-- | Dispatch 'Event'.
onEvent :: Viewer -> Event -> Maybe State -> IO (Maybe State)

-- Dispatch to local handler.
onEvent viewer frame@(Frame _duration _viewport) (Just state) =
    Just <$> onFrame viewer frame state

-- Dispatch to local handler.
onEvent viewer key@(KeyStroke _key _keyState _modifierKeys) (Just state) =
    Just <$> onKeyStroke viewer key state

-- Close.
onEvent viewer CloseRequest (Just state) = do
    close viewer
    return (Just state)

-- Init went wrong, close.
onEvent viewer _ Nothing = do
    close viewer
    return Nothing

-- | Handle the Frame event.
onFrame :: Viewer -> Event -> State -> IO State
onFrame viewer (Frame _ viewport) state = do
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
    return state
onFrame _ _ _ = error "Shall never happen"

-- | Handle the KeyStroke event.
onKeyStroke :: Viewer -> Event -> State -> IO State
onKeyStroke viewer (KeyStroke key keyState _) state = do
    sceneLog viewer <| toLogStr ("KeyStroke: " ++ show key ++ ", " ++ show keyState)
    return state
onKeyStroke _ _ _ = error "Shall never happen"
