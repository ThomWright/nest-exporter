module Web.Nest.HttpClient.Base
  ( Host
  , nestHost
  , defaultSecureRequest
  ) where

import qualified Data.ByteString.Char8 as S8
import           Network.HTTP.Client
import           Network.HTTP.Simple

type Host = S8.ByteString

-- |The main host for API calls (excluding the access token endpoint).
-- Note that this should only be used for the first request. It will be
-- redirected to a Firebase host, and this host should be used from then
-- on.
nestHost :: Host
nestHost = S8.pack "developer-api.nest.com"

defaultSecureRequest :: Request
defaultSecureRequest =
  setRequestSecure True $ setRequestPort 443 $ setRedirectCount 0 defaultRequest

setRedirectCount :: Int -> Request -> Request
setRedirectCount c r = r {redirectCount = c}
