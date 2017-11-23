{-# LANGUAGE TemplateHaskell #-}
module TerraGlide.Environment
    ( Environment
    , terrainHeight
    , terrainColor0
    , terrainColor1
    , terrainColor2
    , terrainColor3
    , init
    ) where

import           Control.Lens (makeLenses)
import           Linear       (V3 (..))
import           Prelude      hiding (init)
import           Scene        (GLfloat)

-- | A collection of values for rendering of the world.
data Environment = Environment
    { _terrainHeight :: !GLfloat
    -- ^ The model space maximun height of the terrain. Scale factor for
    -- generation of perlin values, and the value used for shading of
    -- terrain gradients.
    , _terrainColor0 :: V3 GLfloat
    -- ^ The terrain color gradient component used for the lowest terrain.
    , _terrainColor1 :: V3 GLfloat
    -- ^ The terrain color gradient component used for the next terrain band.
    , _terrainColor2 :: V3 GLfloat
    -- ^ The terrain color gradient component used for the next terrain band.
    , _terrainColor3 :: V3 GLfloat
    -- ^ The terrain color gradient component used for the uppermost terrain band.
    } deriving Show

makeLenses ''Environment

-- | Produce the default environment.
init :: Environment
init =
    Environment
        { _terrainHeight = 200
        , _terrainColor0 = V3 (115 / 255) (69 / 255) (35 / 255)
        , _terrainColor1 = V3 (57 / 255) (118 / 255) (40 / 255)
        , _terrainColor2 = V3 (45 / 255) (58 / 255) (61 / 255)
        , _terrainColor3 = V3 1 1 1
        }
