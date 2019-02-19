{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Web.Nest.HttpClient.AccessToken
  ( getAccessTokenReq
  , AccessTokenResponseBody(..)
  ) where

import           Data.Aeson                  (FromJSON, parseJSON, withObject,
                                              (.:))
import qualified Data.ByteString.Char8       as S8
import           Data.Text                   (Text)
import           GHC.Generics
import           Network.HTTP.Simple
import           Web.Nest.HttpClient.Auth    (NestAuth (..))
import           Web.Nest.HttpClient.Base    (defaultSecureRequest, nestHost)
import           Web.Nest.HttpClient.Request (NestRequest)

nestOauthPath :: String
nestOauthPath = "/oauth2"

nestAccessTokenPath :: String
nestAccessTokenPath = nestOauthPath ++ "/access_token"

getAccessTokenReq :: NestAuth -> NestRequest AccessTokenResponseBody
getAccessTokenReq NestAuth {clientId, clientSecret, code} =
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

data AccessTokenResponseBody = AccessTokenResponseBody
  { accessToken :: Text
  , expiresIn   :: Int
  } deriving (Eq, Generic, Show)

instance FromJSON AccessTokenResponseBody where
  parseJSON =
    withObject "AccessTokenResponseBody" $ \v ->
      AccessTokenResponseBody <$> v .: "access_token" <*> v .: "expires_in"
