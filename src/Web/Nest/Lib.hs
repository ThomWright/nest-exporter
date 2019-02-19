{-# LANGUAGE NamedFieldPuns #-}

module Web.Nest.Lib
  ( doNestStuff
  ) where

import           Data.IORef               (newIORef)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS  (setGlobalManager, tlsManagerSettings)
import           Web.Nest.AccessToken     (getAccessToken)
import           Web.Nest.HttpClient.Auth (NestAuth)
import           Web.Nest.HttpClient.Base (nestHost)

doNestStuff :: NestAuth -> IO ()
doNestStuff nestAuth = do
  setUpHttpClient
  ref <- newIORef nestHost
  accessToken <- getAccessToken ref nestAuth
  print accessToken

setUpHttpClient :: IO ()
setUpHttpClient = do
  manager <- newManager tlsManagerSettings
  setGlobalManager manager
