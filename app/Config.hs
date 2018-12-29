module Config
  ( AppConfig(..)
  , NestConfig(..)
  , parser
  ) where

import           Data.Ini.Config (IniParser, fieldOf, section, string)

data AppConfig = AppConfig
  { nest :: NestConfig
  } deriving (Eq, Show)

data NestConfig = NestConfig
  { clientId     :: String
  , clientSecret :: String
  , pinCode      :: String
  } deriving (Eq, Show)

parser :: IniParser AppConfig
parser = do
  nestCf <-
    section "NEST" $ do
      cid <- fieldOf "clientId" string
      secret <- fieldOf "clientSecret" string
      pin <- fieldOf "pinCode" string
      return NestConfig {clientId = cid, clientSecret = secret, pinCode = pin}
  return AppConfig {nest = nestCf}
