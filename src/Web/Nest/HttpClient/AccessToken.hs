{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Web.Nest.HttpClient.AccessToken
  ( fetchAccessToken
  , AccessTokenResponse(..)
  , AccessTokenSuccess(..)
  ) where

import           Data.Aeson                (FromJSON, Value (Object), parseJSON,
                                            withObject, (.:))
import qualified Data.ByteString.Char8     as S8
import           Data.HashMap.Lazy         (member)
import           Data.Text                 (Text)
import           GHC.Generics
import           Network.HTTP.Client
import           Network.HTTP.Simple
import           Web.Nest.HttpClient.Auth  (NestAuth (..))
import           Web.Nest.HttpClient.Base  (defaultSecureRequest, nestHost)
import           Web.Nest.HttpClient.Error (NestApiError)

nestOauthPath :: String
nestOauthPath = "/oauth2"

nestAccessTokenPath :: String
nestAccessTokenPath = nestOauthPath ++ "/access_token"

-- |Request for an access token
accessTokenRequest :: NestAuth -> Request
accessTokenRequest NestAuth {clientId, clientSecret, code} =
  setRequestHost (S8.pack nestHost) $
  setRequestPath (S8.pack nestAccessTokenPath) $
  setRequestMethod "POST" $
  setRequestBodyURLEncoded
    [ ("client_id", S8.pack clientId)
    , ("client_secret", S8.pack clientSecret)
    , ("code", S8.pack code)
    , ("grant_type", "authorization_code")
    ]
    defaultSecureRequest

fetchAccessToken :: NestAuth -> IO AccessTokenResponse
fetchAccessToken nestAuth = do
  response <- httpJSON (accessTokenRequest nestAuth)
  return (getResponseBody response :: AccessTokenResponse)

data AccessTokenResponse
  = Success AccessTokenSuccess
  | Error NestApiError
  deriving (Eq, Show)

instance FromJSON AccessTokenResponse where
  parseJSON =
    withObject
      "AccessTokenResponse"
      (\v ->
         if member "error" v
           then Error <$> parseJSON (Object v)
           else Success <$> parseJSON (Object v))

data AccessTokenSuccess = AccessTokenSuccess
  { accessToken :: Text
  , expiresIn   :: Int
  } deriving (Eq, Generic, Show)

instance FromJSON AccessTokenSuccess where
  parseJSON =
    withObject "AccessTokenSuccess" $ \v ->
      AccessTokenSuccess <$> v .: "access_token" <*> v .: "expired_in"
