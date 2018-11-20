module Main where

import           Data.Semigroup      ((<>))
import           Lib (someFunc)
import           Options.Applicative (Parser, ParserInfo, execParser, fullDesc,
                                      header, help, helper, info, long, metavar,
                                      progDesc, short, strOption)

main :: IO ()
main = run =<< execParser opts
  where
    run (CmdLineOptions _) = someFunc

newtype CmdLineOptions = CmdLineOptions
  { config :: FilePath
  }

optParser :: Parser CmdLineOptions
optParser =
  CmdLineOptions <$>
  strOption
    (long "config" <> short 'c' <> metavar "CONFIG_FILE" <>
     help "Path to config file")

opts :: ParserInfo CmdLineOptions
opts =
  info
    (helper <*> optParser)
    (fullDesc <> progDesc "Prometheus metrics exporter for the Nest thermostat" <>
     header "nest-exporter - Prometheus metrics for Nest")
