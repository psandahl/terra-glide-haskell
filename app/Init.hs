module Init
    ( configuration
    , onInit
    ) where

import           Flow    ((<|))
import           Options (Options (..))
import           Scene
import           State   (State)

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
onInit :: Viewer -> IO State
onInit _ = return ()

toDisplayMode :: Bool -> DisplayMode
toDisplayMode True  = FullScreen
toDisplayMode False = Windowed 1024 768
