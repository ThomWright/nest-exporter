module Main where

import           Config              (configParser)
import           Data.Ini.Config     (parseIniFile)
import           Data.Semigroup      ((<>))
import qualified Data.Text.IO        as TIO
import           Lib                 (doNestStuff)
import           Options.Applicative (Parser, ParserInfo, execParser, fullDesc,
                                      header, help, helper, info, long, metavar,
                                      progDesc, short, showDefault, strOption,
                                      value)

main :: IO ()
main = do
  (CmdLineOptions filepath) <- execParser opts
  configFile <- TIO.readFile filepath
  case parseIniFile configFile configParser of
    Left err -> putStrLn $ "Error parsing config file: " ++ err
    Right config -> do
      print config
      doNestStuff

newtype CmdLineOptions = CmdLineOptions
  { configFilePath :: FilePath
  }

optParser :: Parser CmdLineOptions
optParser =
  CmdLineOptions <$>
  strOption
    (long "config" <> short 'c' <> metavar "CONFIG_FILE" <> showDefault <>
     value "config.ini" <>
     help "Path to config file")

opts :: ParserInfo CmdLineOptions
opts =
  info
    (helper <*> optParser)
    (fullDesc <> progDesc "Prometheus metrics exporter for the Nest thermostat" <>
     header "nest-exporter - Prometheus metrics for Nest")
