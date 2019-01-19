module Web.Nest.HttpClient.Auth
  ( NestAuth(..)
  ) where

data NestAuth = NestAuth
  { clientId     :: String
  , clientSecret :: String
  , code         :: String
  } deriving (Eq, Show)
