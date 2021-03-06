{-# LANGUAGE TemplateHaskell #-}
module TerraGlide.Environment
    ( Environment
    , terrainHeight
    , waterHeight
    , waterColor
    , terrainColor0
    , terrainColor1
    , terrainColor2
    , terrainColor3
    , ambientLightColor
    , ambientLightStrength
    , sunLightDirection
    , sunLightColor
    , init
    ) where

import           Control.Lens (makeLenses)
import           Flow         ((<|))
import           Linear       (V3 (..), normalize)
import           Prelude      hiding (init)
import           Scene        (GLfloat)

-- | A collection of values for rendering of the world.
data Environment = Environment
    { _terrainHeight        :: !GLfloat
    -- ^ The model space maximun height of the terrain. Scale factor for
    -- generation of perlin values, and the value used for shading of
    -- terrain gradients.

    , _waterHeight          :: !GLfloat
    -- ^ The height of the water in the model.

    , _waterColor           :: !(V3 GLfloat)
    -- ^ The color of the water.

    , _terrainColor0        :: !(V3 GLfloat)
    -- ^ The terrain color gradient component used for the lowest terrain.

    , _terrainColor1        :: !(V3 GLfloat)
    -- ^ The terrain color gradient component used for the next terrain band.

    , _terrainColor2        :: !(V3 GLfloat)
    -- ^ The terrain color gradient component used for the next terrain band.

    , _terrainColor3        :: !(V3 GLfloat)
    -- ^ The terrain color gradient component used for the uppermost terrain band.

    , _ambientLightColor    :: !(V3 GLfloat)
    -- ^ The color of the ambient light.

    , _ambientLightStrength :: !GLfloat
    -- ^ The strength of the ambient light.

    , _sunLightDirection    :: !(V3 GLfloat)
    -- ^ The model space direction towards the sun.

    , _sunLightColor        :: !(V3 GLfloat)
    -- ^ The color of the sun's light.
    } deriving Show

makeLenses ''Environment

-- | Produce the default environment.
init :: Environment
init =
    Environment
        { _terrainHeight = 500
        , _waterHeight = 45
        , _waterColor = V3 0 (5 / 255) (25 / 255)
        , _terrainColor0 = V3 (115 / 255) (69 / 255) (35 / 255)
        , _terrainColor1 = V3 (57 / 255) (118 / 255) (40 / 255)
        , _terrainColor2 = V3 (45 / 255) (58 / 255) (61 / 255)
        , _terrainColor3 = V3 1 1 1
        , _ambientLightColor = V3 1 1 1
        , _ambientLightStrength = 0.2
        , _sunLightDirection = normalize <| V3 (-1) 1 0 -- East as pos z is north.
        , _sunLightColor = V3 (182 / 255) (126 / 255) (91 / 255)
        }
