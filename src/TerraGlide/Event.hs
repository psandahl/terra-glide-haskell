{-# LANGUAGE OverloadedStrings #-}
module TerraGlide.Event
    ( onEvent
    ) where

import           Control.Lens                (set, (.~), (^.))
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
onEvent viewer frame@Frame {} (Just state) =
    Just <$> onFrame viewer frame state

-- Dispatch to local handler.
onEvent viewer mouseButton@MouseButton {} (Just state) =
    Just <$> onMouseButton viewer mouseButton state

-- Dispatch to local handler.
onEvent viewer cursorPos@CursorPos {} (Just state) =
    Just <$> onCursorPos viewer cursorPos state

-- Dispatch to local handler.
onEvent viewer key@KeyStroke {} (Just state) =
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
                                             (state ^. mainCameraNavigation)
                                             (state ^. mainCamera)
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
                    , entityProgram = state ^. dummyProgram
                    , entityMesh =  state ^. dummyMesh
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

-- | Handle the 'MouseButton' event.
onMouseButton :: Viewer -> Event -> State -> IO State
onMouseButton viewer (MouseButton button buttonState _ cursorPos) state = do
    sceneLog viewer <|
        toLogStr ("onMouseButton: " ++ show button ++ ", " ++ show buttonState ++ ", " ++ show cursorPos)

    return state
onMouseButton viewer _ state =
    impossibleEvent viewer state "onMouseButton: Called with impossible arguments"

-- | Handle the 'CursorPos' event.
onCursorPos :: Viewer -> Event -> State -> IO State
onCursorPos viewer (CursorPos cursorPos) state = do
    sceneLog viewer <| toLogStr ("onCursorPos: " ++ show cursorPos)

    return state
onCursorPos viewer _ state =
    impossibleEvent viewer state "onCursorPos: Called with impossible arguments"

-- | Handle the KeyStroke event.
onKeyStroke :: Viewer -> Event -> State -> IO State
onKeyStroke viewer (KeyStroke key keyState _) state = do
    sceneLog viewer <| toLogStr ("KeyStroke: " ++ show key ++ ", " ++ show keyState)

    return $!
        case (key, keyState) of
            (Key'Up, KeyState'Pressed) ->
                mainCameraNavigation . forward .~ True <| state

            (Key'Up, KeyState'Released) ->
                mainCameraNavigation . forward .~ False <| state

            (Key'Down, KeyState'Pressed) ->
                mainCameraNavigation . backward .~ True <| state

            (Key'Down, KeyState'Released) ->
                mainCameraNavigation . backward .~ False <| state

            (Key'Left, KeyState'Pressed) ->
                mainCameraNavigation . turnLeft .~ True <| state

            (Key'Left, KeyState'Released) ->
                mainCameraNavigation . turnLeft .~ False <| state

            (Key'Right, KeyState'Pressed) ->
                mainCameraNavigation . turnRight .~ True <| state

            (Key'Right, KeyState'Released) ->
                mainCameraNavigation . turnRight .~ False <| state

            _ -> state

onKeyStroke viewer _ state =
    impossibleEvent viewer state "onKeyStroke: Called with impossible event"

impossibleEvent :: Viewer -> State -> String -> IO State
impossibleEvent viewer state msg = do
    sceneLog viewer <| toLogStr msg
    close viewer
    return state
