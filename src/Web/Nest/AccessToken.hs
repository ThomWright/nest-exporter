{-# LANGUAGE NamedFieldPuns #-}

module Web.Nest.AccessToken
  ( getAccessToken
  ) where

import           Data.Text                        (Text, pack)
import           Web.Nest.FileCache               (withFileCache)
import           Web.Nest.HttpClient.AccessToken  (AccessTokenResponseBody (..),
                                                   getAccessTokenReq)
import           Web.Nest.HttpClient.Auth         (NestAuth)
import           Web.Nest.HttpClient.Error        (NestError (..), body)
import           Web.Nest.HttpClient.RequestState (HostStateRef, request)

accessTokenFile :: FilePath
accessTokenFile = "access-token.json"

getAccessToken :: HostStateRef -> NestAuth -> IO (Either Text Text)
getAccessToken hostStateRef nestAuth =
  withFileCache accessTokenFile (getAccessTokenFromApi hostStateRef nestAuth)

getAccessTokenFromApi :: HostStateRef -> NestAuth -> IO (Either Text Text)
getAccessTokenFromApi hostStateRef nestAuth = do
  response <- request hostStateRef (getAccessTokenReq nestAuth)
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
