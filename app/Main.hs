module Main
    ( main
    ) where

import           Control.Concurrent (setNumCapabilities)
import           Event              (onEvent)
import           Exit               (onExit)
import           Flow               ((<|))
import           GHC.Conc           (getNumProcessors)
import           Init               (configuration, onInit)
import           Options            (Options (..), parseOptions)
import           Scene              (viewScenes)

main :: IO ()
main = do
    options <- parseOptions
    numProcessors <- getNumProcessors
    setNumCapabilities <| min numProcessors (cores options)
    result <- viewScenes (configuration options) onInit onEvent onExit
    case result of
        Right () -> return ()
        Left err -> putStrLn err
