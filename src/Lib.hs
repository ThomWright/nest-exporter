{-# LANGUAGE NamedFieldPuns #-}

module Lib
  ( doNestStuff
  ) where

import           Nest.AccessToken        (getAccessToken)
import           Nest.Api                (NestAuth)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS (setGlobalManager, tlsManagerSettings)

doNestStuff :: NestAuth -> IO ()
doNestStuff nestAuth = do
  setUpHttpClient
  accessToken <- getAccessToken nestAuth
  print accessToken

setUpHttpClient :: IO ()
setUpHttpClient = do
  manager <- newManager tlsManagerSettings
  setGlobalManager manager
