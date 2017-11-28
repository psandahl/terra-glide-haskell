{-# LANGUAGE OverloadedStrings #-}
module TerraGlide.GUI
    ( GUI
    , init
    , getRearMirrorEntity
    ) where

import           Data.Vector.Storable                (Vector, fromList)
import           Flow                                ((<|))
import           Linear                              (V2 (..), V3 (..))
import           Prelude                             hiding (init)
import           Scene                               hiding (texture)
import qualified Scene.GL.Attribute.VertexWithPosTex as WithPosTex

data GUI = GUI
    { program :: !Program
    , mesh    :: !Mesh
    } deriving Show

init :: Viewer -> IO (Either String GUI)
init viewer = do
    eProgram <- loadProgram viewer
    eMesh <- loadMesh viewer
    case (eProgram, eMesh) of
        (Right program', Right mesh') ->
            return <| Right GUI { program = program', mesh = mesh' }

        (Left err, _) ->
            return <| Left err

        (_, Left err) ->
            return <| Left err

getRearMirrorEntity :: Viewport -> Texture -> GUI -> Entity
getRearMirrorEntity _viewport texture gui =
    Entity
        { entitySettings =
            [ SetDepthFunc Always
            ]
        , entityMesh = mesh gui
        , entityProgram = program gui
        , entityUniforms = [ UniformValue "texture" (0 :: GLint) ]
        , entityTextures = [ TextureBinding texture 0 ]
        }

loadMesh :: Viewer -> IO (Either String Mesh)
loadMesh viewer =
    meshFromRequest viewer <|
        MeshRequest { vertices = boxVertices
                    , indices = boxIndices
                    , primitive = Triangles
                    }

loadProgram :: Viewer -> IO (Either String Program)
loadProgram viewer =
    programFromFiles viewer <|
        ProgramRequest
            [ (Vertex, "resources/shader/textured-gui-box.vert")
            , (Fragment, "resources/shader/textured-gui-box.frag")
            ]
            [ "texture"
            ]

boxVertices :: Vector WithPosTex.Vertex
boxVertices =
    fromList
        [ WithPosTex.Vertex (V3 (-0.5) 0.5 0) (V2 0 1)
        , WithPosTex.Vertex (V3 0.5 0.5 0) (V2 1 1)
        , WithPosTex.Vertex (V3 (-0.5) (-0.5) 0) (V2 0 0)
        , WithPosTex.Vertex (V3 0.5 (-0.5) 0) (V2 1 0)
        ]

boxIndices :: Vector GLuint
boxIndices = fromList [1, 0, 2, 1, 2, 3]
