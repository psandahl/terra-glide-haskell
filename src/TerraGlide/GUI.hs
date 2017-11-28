module TerraGlide.GUI
    ( GUI
    , init
    ) where

import           Data.Vector.Storable                (Vector, fromList)
import           Linear                              (V2 (..), V3 (..))
import           Prelude                             hiding (init)
import           Scene
import qualified Scene.GL.Attribute.VertexWithPosTex as WithPosTex

data GUI = GUI
    { program :: !Program
    , mesh    :: !Mesh
    }

init :: Viewer -> IO (Either String GUI)
init = undefined

boxVertices :: Vector WithPosTex.Vertex
boxVertices =
    fromList
        [ WithPosTex.Vertex (V3 (-0.5) 0.5 0) (V2 0 1)
        ]

boxIndices :: Vector GLuint
boxIndices = fromList [1, 0, 2, 1, 2, 3]
