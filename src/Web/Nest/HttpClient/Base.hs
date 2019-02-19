module Web.Nest.HttpClient.Base
  ( Host
  , nestHost
  , defaultSecureRequest
  ) where

import qualified Data.ByteString.Char8 as S8
import           Network.HTTP.Client
import           Network.HTTP.Simple

type Host = S8.ByteString

nestHost :: Host
nestHost = S8.pack "api.home.nest.com"

defaultSecureRequest :: Request
defaultSecureRequest =
  setRequestSecure True $ setRequestPort 443 $ setRedirectCount 0 defaultRequest

setRedirectCount :: Int -> Request -> Request
setRedirectCount c r = r {redirectCount = c}

data NestAuth = NestAuth
  { clientId     :: String
  , clientSecret :: String
  , code         :: String
  } deriving (Eq, Show)
