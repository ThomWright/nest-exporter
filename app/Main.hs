{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Config                   as Config (AppConfig (..),
                                                     NestConfig (..), clientId,
                                                     clientSecret, nest, parser,
                                                     pinCode)
import           Data.Ini.Config          (parseIniFile)
import           Data.Semigroup           ((<>))
import qualified Data.Text.IO             as TIO
import           Options.Applicative      (Parser, ParserInfo, execParser,
                                           fullDesc, header, help, helper, info,
                                           long, metavar, progDesc, short,
                                           showDefault, strOption, value)
import           Web.Nest.HttpClient.Auth (NestAuth (..))
import           Web.Nest.Lib             (doNestStuff)

main :: IO ()
main = do
  (CmdLineOptions filepath) <- execParser opts
  configFile <- TIO.readFile filepath
  case parseIniFile configFile Config.parser of
    Left err -> putStrLn $ "Error parsing config file: " ++ err
    Right config@AppConfig {nest = NestConfig { Config.clientId
                                              , Config.clientSecret
                                              , Config.pinCode
                                              }} -> do
      print config
      doNestStuff (NestAuth clientId clientSecret pinCode)

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
