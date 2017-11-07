{-# LANGUAGE OverloadedStrings #-}
module TerraGlide.Event
    ( onEvent
    ) where

import           Control.Lens                (set, view)
import           Flow                        ((<|))
import           Linear                      ((!*!))
import           Scene
import qualified Scene.Camera                as Camera
import           Scene.Math                  (Angle (..), mkPerspectiveMatrix)
import           TerraGlide.CameraNavigation (backward, forward, turnLeft,
                                              turnRight)
import qualified TerraGlide.CameraNavigation as CameraNavigation
import           TerraGlide.State            (State (..), dummyMesh,
                                              dummyProgram, mainCamera,
                                              mainCameraNavigation)

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
                                             (view mainCameraNavigation state)
                                             (view mainCamera state)
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
                    , entityProgram = view dummyProgram state
                    , entityMesh =  view dummyMesh state
                    , entityUniforms =
                        [ UniformValue "mvpMatrix" mvpMatrix
                        ]
                    , entityTextures = []
                    }
                ]
            }
    return $! set mainCamera newCamera state
onFrame viewer _ state =
    impossibleEvent viewer state "onFrame: Called with impossible arguments"

-- | Handle the KeyStroke event.
onKeyStroke :: Viewer -> Event -> State -> IO State
onKeyStroke viewer (KeyStroke key keyState _) state = do
    sceneLog viewer <| toLogStr ("KeyStroke: " ++ show key ++ ", " ++ show keyState)

    case (key, keyState) of
        (Key'Up, KeyState'Pressed) ->
            return $! set (mainCameraNavigation . forward) True state

        (Key'Up, KeyState'Released) ->
            return $! set (mainCameraNavigation . forward) False state

        (Key'Down, KeyState'Pressed) ->
            return $! set (mainCameraNavigation . backward) True state

        (Key'Down, KeyState'Released) ->
            return $! set (mainCameraNavigation . backward) False state

        (Key'Left, KeyState'Pressed) ->
            return $! set (mainCameraNavigation . turnLeft) True state

        (Key'Left, KeyState'Released) ->
            return $! set (mainCameraNavigation . turnLeft) False state

        (Key'Right, KeyState'Pressed) ->
            return $! set (mainCameraNavigation . turnRight) True state

        (Key'Right, KeyState'Released) ->
            return $! set (mainCameraNavigation . turnRight) False state

        _ ->  return state

onKeyStroke viewer _ state =
    impossibleEvent viewer state "onKeyStroke: Called with impossible event"

impossibleEvent :: Viewer -> State -> String -> IO State
impossibleEvent viewer state msg = do
    sceneLog viewer <| toLogStr msg
    close viewer
    return state
