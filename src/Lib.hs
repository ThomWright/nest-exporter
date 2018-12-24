module Lib
  ( doNestStuff
  ) where

import           Data.Aeson               (Value)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Char8    as S8
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS  (setGlobalManager, tlsManagerSettings)
import           Network.HTTP.Simple

nestHost :: String
nestHost = "api.home.nest.com"

nestOauthPath :: String
nestOauthPath = "/oauth2"

nestAccessTokenPath :: String
nestAccessTokenPath = nestOauthPath ++ "/access_token"

defaultSecureRequest :: Request
defaultSecureRequest = setRequestSecure True $ setRequestPort 443 defaultRequest

doNestStuff :: IO ()
doNestStuff = do
  manager <- newManager tlsManagerSettings
  setGlobalManager manager
  let request =
        setRequestHost (S8.pack nestHost) $
        setRequestPath (S8.pack nestAccessTokenPath) defaultSecureRequest
  response <- httpJSON request
  putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
  print $ getResponseHeader "Content-Type" response
  print $ encodePretty (getResponseBody response :: Value)
