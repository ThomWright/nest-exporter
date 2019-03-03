{-# LANGUAGE NamedFieldPuns #-}

module Web.Nest.Lib
  ( doNestStuff
  ) where

import           Data.IORef               (newIORef, readIORef)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS  (setGlobalManager, tlsManagerSettings)
import           Web.Nest.AccessToken     (getAccessToken)
import           Web.Nest.HttpClient.Auth (NestAuth)
import           Web.Nest.HttpClient.Base (nestHost)

doNestStuff :: NestAuth -> IO ()
doNestStuff nestAuth = do
  setUpHttpClient
  accessToken <- getAccessToken nestAuth
  ref <- newIORef nestHost
  print accessToken
  -- TODO: get thermostat data
  currentHost <- readIORef ref
  print currentHost

setUpHttpClient :: IO ()
setUpHttpClient = do
  manager <- newManager tlsManagerSettings
  setGlobalManager manager
