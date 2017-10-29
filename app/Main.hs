module Main
    ( main
    ) where

import           Control.Concurrent (setNumCapabilities)
import           Flow               ((<|))
import           GHC.Conc           (getNumProcessors)
import           Scene              (viewScenes)
import           TerraGlide         (Options (..), configuration, onEvent,
                                     onExit, onInit, parseOptions)

main :: IO ()
main = do
    options <- parseOptions
    numProcessors <- getNumProcessors
    setNumCapabilities <| min numProcessors (cores options)
    result <- viewScenes (configuration options) onInit onEvent onExit
    case result of
        Right () -> return ()
        Left err -> putStrLn err
