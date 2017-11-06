{-# LANGUAGE OverloadedStrings #-}
module TerraGlide.Event
    ( onEvent
    ) where

import           Flow                        ((<|))
import           Linear                      ((!*!))
import           Scene
import qualified Scene.Camera                as Camera
import           Scene.Math                  (Angle (..), mkPerspectiveMatrix)
import           TerraGlide.CameraNavigation (CameraNavigation (..))
import qualified TerraGlide.CameraNavigation as CameraNavigation
import           TerraGlide.State            (State (..))

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
onFrame viewer (Frame duration viewport) state = do
    let perspMatrix = mkPerspectiveMatrix (Degrees 45) viewport 1 100
        newCamera = CameraNavigation.animate (realToFrac duration)
                                             (mainCameraNavigation state)
                                             (mainCamera state)
        viewMatrix = Camera.matrix newCamera
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
    return state { mainCamera = newCamera }
onFrame viewer _ state =
    impossibleEvent viewer state "onFrame: Called with impossible arguments"

-- | Handle the KeyStroke event.
onKeyStroke :: Viewer -> Event -> State -> IO State
onKeyStroke viewer (KeyStroke key keyState _) state = do
    sceneLog viewer <| toLogStr ("KeyStroke: " ++ show key ++ ", " ++ show keyState)

    case (key, keyState) of
        (Key'Up, KeyState'Pressed) -> do
            let navigation = mainCameraNavigation state
            return $! state { mainCameraNavigation = navigation { forward = True } }

        (Key'Up, KeyState'Released) -> do
            let navigation = mainCameraNavigation state
            return $! state { mainCameraNavigation = navigation { forward = False } }

        _ ->  return state

onKeyStroke viewer _ state =
    impossibleEvent viewer state "onKeyStroke: Called with impossible event"

impossibleEvent :: Viewer -> State -> String -> IO State
impossibleEvent viewer state msg = do
    sceneLog viewer <| toLogStr msg
    close viewer
    return state
