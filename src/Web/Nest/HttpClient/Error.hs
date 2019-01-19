{-# LANGUAGE DeriveGeneric #-}

module Web.Nest.HttpClient.Error
  ( NestApiError(..)
  ) where

import           Data.Aeson   (FromJSON)
import           Data.Text    (Text)
import           GHC.Generics

data NestApiError = NestApiError
  { error             :: Text
  , instance_id       :: Text
  , error_description :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON NestApiError
