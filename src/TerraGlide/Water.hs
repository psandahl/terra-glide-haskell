{-# LANGUAGE OverloadedStrings #-}
module TerraGlide.Water
    ( Water
    , init
    , getWaterSurface
    ) where

import           Control.Lens                     ((^.))
import           Data.Vector.Storable             (Vector, fromList)
import           Flow                             ((<|))
import           Linear                           (M44, V3 (..), (!*!))
import           Prelude                          hiding (init)
import           Scene
import qualified Scene.GL.Attribute.VertexWithPos as WithPos
import           Scene.Math                       (mkTranslationMatrix)
import           TerraGlide.Environment           (Environment, waterColor,
                                                   waterHeight)

data Water = Water
    { program     :: !Program
    , mesh        :: !Mesh
    , dudvTexture :: !Texture
    } deriving Show

init :: Viewer -> IO (Either String Water)
init viewer = do
    eProgram <- loadProgram viewer
    eMesh <- loadMesh viewer
    eDudvTexture <- loadDudvMap viewer
    case (eProgram, eMesh, eDudvTexture) of
        (Right program', Right mesh', Right dudvTexture') ->
            return <|
                Right Water { program = program'
                            , mesh = mesh'
                            , dudvTexture = dudvTexture'
                            }

        (Left err, _, _) ->
            return <| Left err

        (_, Left err, _) ->
            return <| Left err

        (_, _, Left err) ->
            return <| Left err

getWaterSurface :: M44 GLfloat -> M44 GLfloat -> Texture -> Texture -> Environment -> Water -> Entity
getWaterSurface projMatrix viewMatrix refractionTexture reflectionTexture environment water =
    let modelMatrix = mkTranslationMatrix <| V3 0 (environment ^. waterHeight) 0
        mvpMatrix = projMatrix !*! viewMatrix !*! modelMatrix
    in
        Entity
            { entitySettings = []
            , entityProgram = program water
            , entityMesh = mesh water
            , entityUniforms =
                [ UniformValue "mvpMatrix" mvpMatrix
                , UniformValue "waterColor" <| environment ^. waterColor
                , UniformValue "refractionTexture" (0 :: GLint)
                , UniformValue "reflectionTexture" (1 :: GLint)
                , UniformValue "dudvTexture" (2 :: GLint)
                ]
            , entityTextures =
                [ TextureBinding refractionTexture 0
                , TextureBinding reflectionTexture 1
                , TextureBinding (dudvTexture water) 2
                ]
            }

loadProgram :: Viewer -> IO (Either String Program)
loadProgram viewer =
    programFromFiles viewer <|
        ProgramRequest
            [ (Vertex, "resources/shader/water.vert")
            , (Fragment, "resources/shader/water.frag")
            ]
            [ "mvpMatrix"
            , "waterColor"
            , "refractionTexture"
            , "reflectionTexture"
            , "dudvTexture"
            ]

loadMesh :: Viewer -> IO (Either String Mesh)
loadMesh viewer =
    meshFromRequest viewer <|
        MeshRequest loadVertices loadIndices Triangles

loadVertices :: Vector WithPos.Vertex
loadVertices =
    fromList
        [ WithPos.Vertex <| V3 0 0 0
        , WithPos.Vertex <| V3 1024 0 0
        , WithPos.Vertex <| V3 0 0 1024
        , WithPos.Vertex <| V3 1024 0 1024
        ]

loadIndices :: Vector GLuint
loadIndices =
    fromList [ 1, 0, 2, 1, 2, 3 ]

loadDudvMap :: Viewer -> IO (Either String Texture)
loadDudvMap viewer =
    textureFromRequest viewer <|
        defaultTextureRequest "resources/texture/waterDUDV.png"
