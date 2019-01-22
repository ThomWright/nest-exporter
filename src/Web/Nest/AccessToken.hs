{-# LANGUAGE NamedFieldPuns #-}

module Web.Nest.AccessToken
  ( getAccessToken
  ) where

import           Data.Text                       (Text, pack)
import           Web.Nest.FileCache              (withFileCache)
import           Web.Nest.HttpClient.AccessToken (AccessTokenResponseBody (..),
                                                  getAccessTokenReq)
import           Web.Nest.HttpClient.Auth        (NestAuth)
import           Web.Nest.HttpClient.Error       (NestError (..), body)
import           Web.Nest.HttpClient.Request     (sendRequest)

accessTokenFile :: FilePath
accessTokenFile = "access-token.json"

getAccessToken :: NestAuth -> IO (Either Text Text)
getAccessToken nestAuth =
  withFileCache accessTokenFile (getAccessTokenFromApi nestAuth)

getAccessTokenFromApi :: NestAuth -> IO (Either Text Text)
getAccessTokenFromApi nestAuth = do
  response <- sendRequest (getAccessTokenReq nestAuth)
  case response of
    Left e ->
      case e of
        ApiError ae ->
          return
            (Left
               (("Error fetching access token from API: " :: Text) <>
                (pack . show . body) ae))
        ParseFailure pf -> return (Left (("ParseFailure: " :: Text) <> pf))
        UnknownStatusCode status ->
          return
            (Left (("UnknownStatusCode: " :: Text) <> (pack $ show status)))
    Right at -> do
      let AccessTokenResponseBody {accessToken} = at
      return (Right accessToken)
