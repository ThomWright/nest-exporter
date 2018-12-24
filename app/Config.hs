module Config
  ( Config
  , NestConfig
  , configParser
  ) where

import           Data.Ini.Config (IniParser, fieldOf, section, string)

data Config = Config
  { nest :: NestConfig
  } deriving (Eq, Show)

data NestConfig = NestConfig
  { clientId     :: String
  , clientSecret :: String
  , pinCode      :: String
  } deriving (Eq, Show)

configParser :: IniParser Config
configParser = do
  nestCf <-
    section "NEST" $ do
      cid <- fieldOf "clientId" string
      secret <- fieldOf "clientSecret" string
      pin <- fieldOf "pinCode" string
      return NestConfig {clientId = cid, clientSecret = secret, pinCode = pin}
  return Config {nest = nestCf}
