{-# LANGUAGE OverloadedStrings #-}
module TerraGlide.Terrain
    ( Terrain
    , init
    , getEntities
    ) where

import           Flow                  ((<|))
import           Linear                (M44, V3, (!*!))
import           Prelude               hiding (init)
import           Scene
import           Scene.PerlinGenerator (GeneratorContext)
import qualified Scene.PerlinGenerator as Gen

data Terrain = Terrain
    { genContext :: !GeneratorContext
    , program    :: !Program
    , mesh       :: !Mesh
    } deriving Show

init :: Viewer -> V3 GLfloat -> IO (Either String Terrain)
init viewer _startPos = do
    let context = loadGeneratorContext
    eProgram <- loadTerrainProgram viewer
    eMesh <- loadDummyTileMesh viewer context

    case (eProgram, eMesh) of
        (Right program', Right mesh') ->
            return <| Right Terrain { genContext = context, program = program', mesh = mesh' }

        (Left err, _) -> return <| Left err
        (_, Left err) -> return <| Left err

getEntities :: Viewer -> V3 GLfloat -> M44 GLfloat -> M44 GLfloat -> Terrain -> IO [Entity]
getEntities _viewer _currentPos proj view terrain = do
    let mvpMatrix = proj !*! view
    return [ Entity
                { entitySettings = []
                , entityProgram = program terrain
                , entityMesh =  mesh terrain
                , entityUniforms =
                    [ UniformValue "mvpMatrix" mvpMatrix
                    ]
                , entityTextures = []
                }
           ]

loadGeneratorContext :: GeneratorContext
loadGeneratorContext =
    Gen.defaultGeneratorContext { Gen.weights = Gen.softTerrain }

loadTerrainProgram :: Viewer -> IO (Either String Program)
loadTerrainProgram viewer =
    programFromFiles viewer <|
        ProgramRequest [ (Vertex, "resources/shader/terrain-tile.vert")
                       , (Fragment, "resources/shader/terrain-tile.frag")
                       ] ["mvpMatrix"]

loadDummyTileMesh :: Viewer -> GeneratorContext -> IO (Either String Mesh)
loadDummyTileMesh viewer context = do
    let query =
            Gen.GeneratorQuery
                { Gen.xPos = 0
                , Gen.yPos = 0
                , Gen.width = 256
                , Gen.height = 256
                , Gen.scale = 10
                }
        tileData = Gen.genSmoothTerrain context query
    meshFromRequest viewer <|
        MeshRequest (Gen.vertices tileData) (Gen.indices tileData) Triangles
