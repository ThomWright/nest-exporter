{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Nest.Api
  ( NestAuth(..)
  , fetchAccessToken
  , AccessTokenResponse(..)
  , AccessTokenSuccess(..)
  , NestApiError(..)
  ) where

import           Data.Aeson            (FromJSON, Value (Object), parseJSON,
                                        withObject, (.:))
import qualified Data.Aeson.Types      as AesonTypes
import qualified Data.ByteString.Char8 as S8
import           Data.HashMap.Lazy     (member)
import           Data.Text             (Text)
import           GHC.Generics
import           Network.HTTP.Client
import           Network.HTTP.Simple

nestHost :: String
nestHost = "api.home.nest.com"

nestOauthPath :: String
nestOauthPath = "/oauth2"

nestAccessTokenPath :: String
nestAccessTokenPath = nestOauthPath ++ "/access_token"

defaultSecureRequest :: Request
defaultSecureRequest = setRequestSecure True $ setRequestPort 443 defaultRequest

data NestAuth = NestAuth
  { clientId     :: String
  , clientSecret :: String
  , code         :: String
  } deriving (Eq, Show)

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
      (\v -> parseAccessTokenResponse (member "error" v) (Object v))

parseAccessTokenResponse ::
     Bool -> Value -> AesonTypes.Parser AccessTokenResponse
parseAccessTokenResponse hasError value
  | hasError = Error <$> parseJSON value
  | otherwise = Success <$> parseJSON value

data AccessTokenSuccess = AccessTokenSuccess
  { accessToken :: Text
  , expiresIn   :: Int
  } deriving (Eq, Generic, Show)

instance FromJSON AccessTokenSuccess where
  parseJSON =
    withObject "AccessTokenSuccess" $ \v ->
      AccessTokenSuccess <$> v .: "access_token" <*> v .: "expired_in"

data NestApiError = NestApiError
  { error             :: Text
  , instance_id       :: Text
  , error_description :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON NestApiError
