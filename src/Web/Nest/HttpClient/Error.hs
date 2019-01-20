{-# LANGUAGE NamedFieldPuns #-}

module Web.Nest.HttpClient.Error
  ( NestApiErrorBody(..)
  , NestApiError(..)
  , NestError(..)
  ) where

import           Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import           Data.Text  (Text, unpack)

data NestError
  = ApiError NestApiError
  | ParseFailure Text
  | UnknownStatusCode Int
  deriving (Eq, Show)

data NestApiError = NestApiError
  { status :: Int
  , body   :: NestApiErrorBody
  } deriving (Eq, Show)

data NestApiErrorBody = NestApiErrorBody
  { short      :: Text
  , long       :: Text
  , url        :: Text
  , instanceId :: Text
  } deriving (Eq)

instance Show NestApiErrorBody where
  show NestApiErrorBody {short, long, url} =
    "Error from the Nest API: " ++
    (unpack short) ++
    "\n\n" ++ (unpack long) ++ "\n\n" ++ "See: " ++ (unpack url)

instance FromJSON NestApiErrorBody where
  parseJSON =
    withObject "NestApiErrorBody" $ \v ->
      NestApiErrorBody <$> v .: "error" <*> v .: "message" <*> v .: "type" <*>
      v .: "instance"
