{-# LANGUAGE OverloadedStrings #-}
module TerraGlide.Terrain
    ( Terrain
    , init
    , getEntities
    ) where

import           Control.Lens           ((^.))
import           Flow                   ((<|))
import           Linear                 (M44, V3 (..), normalize, (!*!))
import           Prelude                hiding (init)
import           Scene
import           Scene.Math             (Application (Vector), apply,
                                         normalMatrix)
import           Scene.PerlinGenerator  (GeneratorContext)
import qualified Scene.PerlinGenerator  as Gen
import           TerraGlide.Environment (Environment, ambientLightColor,
                                         ambientLightStrength, sunLightColor,
                                         sunLightDirection, terrainColor0,
                                         terrainColor1, terrainColor2,
                                         terrainColor3, terrainHeight)

data Terrain = Terrain
    { genContext :: !GeneratorContext
    , program    :: !Program
    , mesh       :: !Mesh
    } deriving Show

init :: Viewer -> Environment -> V3 GLfloat -> IO (Either String Terrain)
init viewer environment _startPos = do
    let context = loadGeneratorContext
    eProgram <- loadTerrainProgram viewer
    eMesh <- loadDummyTileMesh viewer environment context

    case (eProgram, eMesh) of
        (Right program', Right mesh') ->
            return <| Right Terrain { genContext = context, program = program', mesh = mesh' }

        (Left err, _) -> return <| Left err
        (_, Left err) -> return <| Left err

getEntities :: M44 GLfloat -> M44 GLfloat -> Environment -> Terrain -> [Entity]
getEntities proj view environment terrain =
    let mvpMatrix = proj !*! view -- Note: the will likely be a model matrix as well.
        transformedSunLightDirection = transformSunLight view <| environment ^. sunLightDirection
    in
        [ Entity
            { entitySettings = []
            , entityProgram = program terrain
            , entityMesh =  mesh terrain
            , entityUniforms =
                [ UniformValue "mvpMatrix" mvpMatrix
                , UniformValue "normalMatrix" <| normalMatrix view -- Note: no model matrix
                , UniformValue "terrainHeight" <| environment ^. terrainHeight
                , UniformValue "terrainColor0" <| environment ^. terrainColor0
                , UniformValue "terrainColor1" <| environment ^. terrainColor1
                , UniformValue "terrainColor2" <| environment ^. terrainColor2
                , UniformValue "terrainColor3" <| environment ^. terrainColor3
                , UniformValue "ambientLightColor" <| environment ^. ambientLightColor
                , UniformValue "ambientLightStrength" <| environment ^. ambientLightStrength
                , UniformValue "transformedSunLightDirection" transformedSunLightDirection
                , UniformValue "sunLightColor" <| environment ^. sunLightColor
                ]
            , entityTextures = []
            }
        ]

transformSunLight :: M44 GLfloat -> V3 GLfloat -> V3 GLfloat
transformSunLight view = normalize . apply view . Vector

loadGeneratorContext :: GeneratorContext
loadGeneratorContext =
    Gen.defaultGeneratorContext { Gen.weights = Gen.rockyTerrain }

loadTerrainProgram :: Viewer -> IO (Either String Program)
loadTerrainProgram viewer =
    programFromFiles viewer <|
        ProgramRequest [ (Vertex, "resources/shader/terrain-tile.vert")
                       , (Fragment, "resources/shader/terrain-tile.frag")
                       ]
                       [ "mvpMatrix"
                       , "normalMatrix"
                       , "terrainHeight"
                       , "terrainColor0"
                       , "terrainColor1"
                       , "terrainColor2"
                       , "terrainColor3"
                       , "ambientLightColor"
                       , "ambientLightStrength"
                       , "transformedSunLightDirection"
                       , "sunLightColor"
                       ]

loadDummyTileMesh :: Viewer -> Environment -> GeneratorContext -> IO (Either String Mesh)
loadDummyTileMesh viewer environment context = do
    let query =
            Gen.GeneratorQuery
                { Gen.xPos = 3900
                , Gen.yPos = 3000
                , Gen.width = 256 * 4
                , Gen.height = 256 * 4
                , Gen.scale = round <| environment ^. terrainHeight
                }
        tileData = Gen.genSmoothTerrain context query
    meshFromRequest viewer <|
        MeshRequest (Gen.vertices tileData) (Gen.indices tileData) Triangles
