{-# LANGUAGE NamedFieldPuns #-}

module Nest.AccessToken
(getAccessToken)
where

import           Data.Aeson              (decode, encode)
import qualified Data.ByteString.Lazy    as LBS
import           Data.Text               (Text)
import           Nest.Api                    (AccessTokenResponse (..),
                                          AccessTokenSuccess (..), NestAuth,
                                          error_description, fetchAccessToken)
import           Control.Exception       (tryJust)
import           Control.Monad           (guard)
import           System.IO.Error         (isDoesNotExistError)

accessTokenFile :: FilePath
accessTokenFile = "access-token.json"

getAccessToken :: NestAuth -> IO (Either Text Text)
getAccessToken nestAuth
  -- try reading access token from cache file
  = do
  cacheFileContents <- readJsonFile accessTokenFile
  case cacheFileContents of
    Right accessToken -> return (Right accessToken)
    Left e -> do
      print ("Error reading access token cache file: " ++ show e)
      -- no cache, get it from the API
      getAccessTokenFromApi nestAuth

getAccessTokenFromApi :: NestAuth -> IO (Either Text Text)
getAccessTokenFromApi nestAuth = do
  response <- fetchAccessToken nestAuth
  case response of
    Error e ->
      return $
      Left
        (("Error fetching access token from API: " :: Text) <>
          error_description e)
    Success a -> do
      let AccessTokenSuccess {accessToken} = a
      -- write access token to cache file
      LBS.writeFile accessTokenFile (encode accessToken)
      return (Right accessToken)

readJsonFile :: FilePath -> IO (Either String Text)
readJsonFile filepath = do
  fileContents <- tryJust (guard . isDoesNotExistError) (LBS.readFile filepath)
  case fileContents of
    Right fileContents' ->
      case decode fileContents' of
        Just decoded -> return (Right decoded)
        Nothing      -> return (Left "Unparseable JSON")
    Left _ -> return (Left "File not found")
  