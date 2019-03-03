{-# LANGUAGE NamedFieldPuns #-}

module Web.Nest.AccessToken
  ( getAccessToken
  ) where

import           Data.Text                       (Text, pack)

import           Network.HTTP.Client             (host)
import           Web.Nest.FileCache              (withFileCache)
import           Web.Nest.HttpClient.AccessToken (AccessTokenResponseBody (..),
                                                  getAccessTokenReq)
import           Web.Nest.HttpClient.Auth        (NestAuth)
import           Web.Nest.HttpClient.Error       (NestError (..), body)
import           Web.Nest.HttpClient.Request     (RedirectOrResponse (..),
                                                  sendRequest)

accessTokenFile :: FilePath
accessTokenFile = "access-token.json"

getAccessToken :: NestAuth -> IO (Either Text Text)
getAccessToken nestAuth =
  withFileCache accessTokenFile (getAccessTokenFromApi nestAuth)

getAccessTokenFromApi :: NestAuth -> IO (Either Text Text)
getAccessTokenFromApi nestAuth = do
  let req = getAccessTokenReq nestAuth
  -- the access token request has its own host
  r <- sendRequest (host req) req
  case r of
    Redirect _ -> return (Left "Unexpected redirect while getting access token")
    Response response ->
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
