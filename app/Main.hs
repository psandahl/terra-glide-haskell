module Main
    ( main
    ) where

import           Control.Concurrent  (setNumCapabilities)
import           Data.Semigroup      ((<>))
import           Flow                ((<|))
import           GHC.Conc            (getNumProcessors)
import           Options.Applicative (Parser, ParserInfo, auto, execParser,
                                      fullDesc, header, help, helper, info,
                                      long, option, progDesc, short,
                                      showDefault, switch, value, (<**>))

data Options = Options
    { fullScreen :: !Bool
    , debug      :: !Bool
    , cores      :: !Int
    } deriving Show

main :: IO ()
main = do
    options <- parseOptions
    numProcessors <- getNumProcessors
    setNumCapabilities <| min numProcessors (cores options)

parseOptions :: IO Options
parseOptions = execParser parserInfo

parserInfo :: ParserInfo Options
parserInfo =
    info (parser <**> helper)
        ( fullDesc
        <> progDesc "Procedural 3D terrain simulation"
        <> header "terra-glide"
        )

parser :: Parser Options
parser = Options
    <$> switch (
        long "fullscreen"
     <> short 'f'
     <> help "Run in fullscreen mode"
    )
    <*> switch (
        long "debug"
     <> short 'd'
     <> help "Run with debug logging"
    )
    <*> option auto (
        long "cores"
     <> short 'c'
     <> help "The number of cores to utilize"
     <> value 2
     <> showDefault
    )
