{-# LANGUAGE OverloadedStrings #-}
module TerraGlide.Event
    ( onEvent
    ) where

import           Control.Lens                (set, (%~), (.~), (^.))
import           Control.Monad               (when)
import           Flow                        ((<|))
import           Linear                      (_x, _y, _z)
import           Scene
import           Scene.Camera                (Camera)
import qualified Scene.Camera                as Camera
import           Scene.Math                  (Angle (..), mkPerspectiveMatrix)
import           TerraGlide.CameraNavigation (backward, down, forward,
                                              lastCursorPos, turnLeft,
                                              turnRight, up)
import qualified TerraGlide.CameraNavigation as CameraNavigation
import           TerraGlide.State            (State (..), debug, environment,
                                              mainCamera, mainCameraNavigation,
                                              terrain)
import qualified TerraGlide.Terrain          as Terrain
import           Text.Printf                 (printf)

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

-- | Handle the Frame event. Animate stuff and construct the 'SceneGraph'.
onFrame :: Viewer -> Event -> State -> IO State
onFrame viewer (Frame duration viewport) state = do
    let perspMatrix = mkPerspectiveMatrix (Degrees 45) viewport 0.1 2000
        newCamera = CameraNavigation.animate (realToFrac duration)
                                             (state ^. mainCameraNavigation)
                                             (state ^. mainCamera)
        viewMatrix = Camera.matrix newCamera
        terrainEntities =
            Terrain.getEntities perspMatrix viewMatrix (state ^. environment) (state ^. terrain)

    debugCamera viewer state newCamera

    setSceneGraph viewer <|
        SceneGraph
            { sceneGraphSettings =
                [ Clear [ColorBufferBit, DepthBufferBit]
                ]
            , firstScene = Just <|
                Scene { sceneSettings = []
                      , sceneRenderBuffer = Nothing
                      , sceneEntities = terrainEntities
                      , nextScene = Nothing
                      }
            }
    return $! set mainCamera newCamera state

-- This shall never happen.
onFrame viewer _ state =
    impossibleEvent viewer state "onFrame: Called with impossible arguments"

-- | Handle the 'MouseButton' event.
onMouseButton :: Viewer -> Event -> State -> IO State
onMouseButton viewer (MouseButton button buttonState _ cursorPos) state = do
    debugLog viewer state <|
        "onMouseButton: " ++ show button ++ ", " ++ show buttonState ++ ", " ++ show cursorPos

    return $!
        case (button, buttonState) of
            (MouseButton'1, MouseButtonState'Pressed) ->
                mainCameraNavigation . lastCursorPos .~ Just cursorPos <| state

            (MouseButton'1, MouseButtonState'Released) ->
                mainCameraNavigation . lastCursorPos .~ Nothing <| state

            _ -> state
onMouseButton viewer _ state =
    impossibleEvent viewer state "onMouseButton: Called with impossible arguments"

-- | Handle the 'CursorPos' event. Change the view of the camera and then update
-- the last cursor position.
onCursorPos :: Viewer -> Event -> State -> IO State
onCursorPos viewer (CursorPos newCursorPos) state = do
    debugLog viewer state <| "onCursorPos: " ++ show newCursorPos

    return $!
        case state ^. mainCameraNavigation . lastCursorPos of
            Just oldCursorPos ->
                mainCameraNavigation . lastCursorPos .~ Just newCursorPos <|
                    mainCamera %~ CameraNavigation.changeView newCursorPos oldCursorPos
                        <| state

            Nothing -> state
onCursorPos viewer _ state =
    impossibleEvent viewer state "onCursorPos: Called with impossible arguments"

-- | Handle the KeyStroke event.
onKeyStroke :: Viewer -> Event -> State -> IO State
onKeyStroke viewer (KeyStroke key keyState _) state = do
    debugLog viewer state <| "onKeyStroke: " ++ show key ++ ", " ++ show keyState

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

            (Key'PageUp, KeyState'Pressed) ->
                mainCameraNavigation . up .~ True <| state

            (Key'PageUp, KeyState'Released) ->
                mainCameraNavigation . up .~ False <| state

            (Key'PageDown, KeyState'Pressed) ->
                mainCameraNavigation . down .~ True <| state

            (Key'PageDown, KeyState'Released) ->
                mainCameraNavigation . down .~ False <| state

            _ -> state

onKeyStroke viewer _ state =
    impossibleEvent viewer state "onKeyStroke: Called with impossible event"

impossibleEvent :: Viewer -> State -> String -> IO State
impossibleEvent viewer state msg = do
    sceneLog viewer <| toLogStr msg
    close viewer
    return state

debugLog :: Viewer -> State -> String -> IO ()
debugLog viewer state str =
    when (state ^. debug) <|
        sceneLog viewer (toLogStr str)

debugCamera :: Viewer -> State -> Camera -> IO ()
debugCamera viewer state camera =
    when (CameraNavigation.anyActive <| state ^. mainCameraNavigation) $ do
        let str = printf "Camera: x=%f y=%f z=%f"
                        (camera ^. Camera.position . _x)
                        (camera ^. Camera.position . _y)
                        (camera ^. Camera.position . _z)
        debugLog viewer state str
