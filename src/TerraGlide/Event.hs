{-# LANGUAGE OverloadedStrings #-}
module TerraGlide.Event
    ( onEvent
    ) where

import           Control.Lens                ((%~), (.~), (^.))
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
import           TerraGlide.Environment      (Environment, waterHeight)
import qualified TerraGlide.GUI              as GUI
import           TerraGlide.State            (State (..), debug, environment,
                                              gui, mainCamera,
                                              mainCameraNavigation,
                                              reflectionFramebuffer,
                                              refractionFramebuffer, terrain,
                                              water)
import qualified TerraGlide.Terrain          as Terrain
import qualified TerraGlide.Water            as Water
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
-- TODO: Clean it up!
onFrame :: Viewer -> Event -> State -> IO State
onFrame viewer (Frame duration viewport) state = do
    -- Calculate some values. Start by picking out the refraction framebuffer.
    let refraction = state ^. refractionFramebuffer

        -- Pick out the reflection framebuffer.
        reflection = state ^. reflectionFramebuffer

        -- Calculate the main projection matrix, the projection used for
        -- the rendering of the the main scene.
        mainProjMatrix = mkPerspectiveMatrix (Degrees 45) viewport 2 2000

        -- Calculate the framebuffer projection matrix, used for refraction and
        -- reflection rendering. Assume they have the same size.
        fbProjMatrix = mkPerspectiveMatrix (Degrees 45)
                                           (framebufferViewport refraction)
                                           2 2000

        -- Now, move the camera proportional to the frame duration.
        newCamera = CameraNavigation.animate (realToFrac duration)
                                             (state ^. mainCameraNavigation)
                                             (state ^. mainCamera)

        -- Also animate the water.
        newWater = Water.animate (realToFrac duration) <| state ^. water

        -- We need a separate underwater camera for the reflection rendering.
        reflectionCamera = mkUnderwaterCamera (state ^. environment) newCamera

        -- Calculate the main view matrix.
        mainViewMatrix = Camera.matrix newCamera

        -- Calculate the reflection view matrix.
        reflectionViewMatrix = Camera.matrix reflectionCamera

        -- Get the standard terrain rendering.
        standardTerrain = Terrain.getStandardTerrain mainProjMatrix
                                                     mainViewMatrix
                                                     (state ^. environment)
                                                     (state ^. terrain)

        -- Get the refraction terrain rendering.
        refractionTerrain = Terrain.getRefractionTerrain fbProjMatrix
                                                         mainViewMatrix
                                                         (state ^. environment)
                                                         (state ^. terrain)

        -- Get the reflection terrain rendering.
        reflectionTerrain = Terrain.getReflectionTerrain fbProjMatrix
                                                         reflectionViewMatrix
                                                         (state ^. environment)
                                                         (state ^. terrain)

        -- Get the water surface.
        waterSurface = Water.getWaterSurface mainProjMatrix
                                             mainViewMatrix
                                             (colorTexture refraction)
                                             (colorTexture reflection)
                                             (state ^. environment)
                                             newWater

        textureDisplay = GUI.getTextureDisplay (framebufferViewport refraction)
                                               viewport (colorTexture reflection)
                                               (state ^. gui)

    -- Log the camera position.
    debugCamera viewer state newCamera

    -- Construct the new 'Scene'. Three renderings.
    setScene viewer <|
        Scene
            { sceneSettings = []
            , firstRendering = Just <|
                Rendering -- Rendering of the refraction texture.
                    { renderingSettings =
                        [ Clear [ColorBufferBit, DepthBufferBit]
                        ]
                    , renderingBuffer = Just refraction
                    , renderingEntities = refractionTerrain
                    , nextRendering = Just <|
                        Rendering -- Rendering of the reflection texture.
                            { renderingSettings =
                                [ Clear [ColorBufferBit, DepthBufferBit]
                                ]
                            , renderingBuffer = Just reflection
                            , renderingEntities = reflectionTerrain
                            , nextRendering = Just <|
                                Rendering -- Rendering of the final display view.
                                    { renderingSettings =
                                        [ Clear [ColorBufferBit, DepthBufferBit]
                                        ]
                                        , renderingBuffer = Nothing
                                        , renderingEntities = standardTerrain ++ [waterSurface, textureDisplay]
                                        , nextRendering = Nothing
                                    }
                            }
                    }
            }

    -- We're done. Update the 'State' with stuff that need to be updated.
    return $! mainCamera .~ newCamera <| water .~ newWater <| state

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

mkUnderwaterCamera :: Environment -> Camera -> Camera
mkUnderwaterCamera env camera =
    let diff = camera ^. Camera.position . _y - env ^. waterHeight
    in Camera.flipViewElevation <| Camera.down (2 * diff) camera
