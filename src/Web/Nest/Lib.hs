{-# LANGUAGE NamedFieldPuns #-}

module Web.Nest.Lib
  ( doNestStuff
  ) where

import           Data.IORef                       (newIORef)
import           Data.Text                        (unpack)
import           Network.HTTP.Client              (newManager)
import           Network.HTTP.Client.TLS          (setGlobalManager,
                                                   tlsManagerSettings)
import           Web.Nest.AccessToken             (getAccessToken)
import           Web.Nest.HttpClient.Auth         (NestAuth)
import           Web.Nest.HttpClient.Base         (nestHost)
import           Web.Nest.HttpClient.RequestState (RedirectState (..), Request,
                                                   request)
import           Web.Nest.HttpClient.Thermostat   (ThermostatResponseBody,
                                                   getThermostatData)

type ThermostatReq = Request ThermostatResponseBody

doNestStuff :: NestAuth -> IO ()
doNestStuff nestAuth = do
  setUpHttpClient
  accessTokenResult <- getAccessToken nestAuth
  case accessTokenResult of
    Left err -> print ("Error getting access token: " ++ unpack err)
    Right accessToken -> do
      -- print accessToken
      ref <- newIORef (RedirectState nestHost 443)
      let thermostatReq =
            getThermostatData accessToken "8Jseryl9Czye4Z5je-QQNplzKN1nbeYK"
      thermostatData <- (request :: ThermostatReq) ref thermostatReq
      print thermostatData

setUpHttpClient :: IO ()
setUpHttpClient = do
  manager <- newManager tlsManagerSettings
  setGlobalManager manager
