{-# LANGUAGE NamedFieldPuns #-}

module Web.Nest.AccessToken
  ( getAccessToken
  ) where

import           Control.Exception       (tryJust)
import           Control.Monad           (guard)
import           Data.Aeson              (decode, encode)
import qualified Data.ByteString.Lazy    as LBS
import           Data.Text               (Text)
import           System.IO.Error         (isDoesNotExistError)
import           Web.Nest.HttpClient.Api (AccessTokenResponse (..),
                                          AccessTokenSuccess (..), NestAuth,
                                          error_description, fetchAccessToken)

accessTokenFile :: FilePath
accessTokenFile = "access-token.json"

getAccessToken :: NestAuth -> IO (Either Text Text)
getAccessToken nestAuth = do
  cacheFileContents <- readJsonFile accessTokenFile
  case cacheFileContents of
    AccessToken accessToken -> return (Right accessToken)
    UnparsableJSON ->
      return (Left "Unparsable JSON in cached access token file")
    FileNotFound -> do
      putStrLn
        "No cache file found, fetching from API using one-time authorization code"
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

readJsonFile :: FilePath -> IO CacheFileResult
readJsonFile filepath = do
  fileContents <- tryJust (guard . isDoesNotExistError) (LBS.readFile filepath)
  case fileContents of
    Right fileContents' ->
      case decode fileContents' of
        Just decoded -> return (AccessToken decoded)
        Nothing      -> return UnparsableJSON
    Left _ -> return FileNotFound

data CacheFileResult
  = FileNotFound
  | UnparsableJSON
  | AccessToken Text
  deriving (Eq, Show)
