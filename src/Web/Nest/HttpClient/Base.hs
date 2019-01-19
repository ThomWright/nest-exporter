module Web.Nest.HttpClient.Base
  ( nestHost
  , defaultSecureRequest
  ) where

import           Network.HTTP.Client
import           Network.HTTP.Simple

nestHost :: String
nestHost = "api.home.nest.com"

defaultSecureRequest :: Request
defaultSecureRequest = setRequestSecure True $ setRequestPort 443 defaultRequest

data NestAuth = NestAuth
  { clientId     :: String
  , clientSecret :: String
  , code         :: String
  } deriving (Eq, Show)
