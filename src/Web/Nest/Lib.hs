{-# LANGUAGE NamedFieldPuns #-}

module Web.Nest.Lib
  ( doNestStuff
  ) where

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS (setGlobalManager, tlsManagerSettings)
import           Web.Nest.AccessToken    (getAccessToken)
import           Web.Nest.HttpClient.Api (NestAuth)

doNestStuff :: NestAuth -> IO ()
doNestStuff nestAuth = do
  setUpHttpClient
  accessToken <- getAccessToken nestAuth
  print accessToken

setUpHttpClient :: IO ()
setUpHttpClient = do
  manager <- newManager tlsManagerSettings
  setGlobalManager manager
