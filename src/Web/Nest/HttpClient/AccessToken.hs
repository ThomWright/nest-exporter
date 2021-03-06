{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Web.Nest.HttpClient.AccessToken
  ( AccessToken(..)
  , getAccessTokenReq
  , AccessTokenResponseBody(..)
  ) where

import           Data.Aeson                  (FromJSON, ToJSON, Value (String),
                                              parseJSON, toJSON, withObject,
                                              withText, (.:))
import qualified Data.ByteString.Char8       as S8
import           Data.Text                   (Text)
import           GHC.Generics
import           Network.HTTP.Simple
import           Web.Nest.HttpClient.Auth    (NestAuth (..))
import           Web.Nest.HttpClient.Base    (Host, defaultSecureRequest)
import           Web.Nest.HttpClient.Request (NestRequest)

-- |This is a different host from the rest of the API
nestOauthHost :: Host
nestOauthHost = S8.pack "api.home.nest.com"

nestOauthPath :: S8.ByteString
nestOauthPath = S8.pack "/oauth2"

nestAccessTokenPath :: S8.ByteString
nestAccessTokenPath = nestOauthPath <> (S8.pack "/access_token")

getAccessTokenReq :: NestAuth -> NestRequest AccessTokenResponseBody
getAccessTokenReq NestAuth {clientId, clientSecret, code} =
  setRequestHost nestOauthHost $
  setRequestPath nestAccessTokenPath $
  setRequestMethod "POST" $
  setRequestBodyURLEncoded
    [ ("client_id", S8.pack clientId)
    , ("client_secret", S8.pack clientSecret)
    , ("code", S8.pack code)
    , ("grant_type", "authorization_code")
    ]
    defaultSecureRequest

newtype AccessToken =
  AccessToken Text
  deriving (Eq, Generic, Show)

instance FromJSON AccessToken where
  parseJSON = withText "AccessToken" $ pure . AccessToken

instance ToJSON AccessToken where
  toJSON (AccessToken t) = String t

data AccessTokenResponseBody = AccessTokenResponseBody
  { accessToken :: AccessToken
  , expiresIn   :: Int
  } deriving (Eq, Generic, Show)

instance FromJSON AccessTokenResponseBody where
  parseJSON =
    withObject "AccessTokenResponseBody" $ \v ->
      AccessTokenResponseBody <$> (AccessToken <$> v .: "access_token") <*>
      v .: "expires_in"
