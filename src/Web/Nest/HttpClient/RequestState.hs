{-# LANGUAGE NamedFieldPuns #-}

module Web.Nest.HttpClient.RequestState
  ( request
  , Request
  , RedirectState(..)
  , RedirectStateRef
  ) where

import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson                  (FromJSON)

import           Data.IORef                  (IORef, readIORef, writeIORef)
import           Network.HTTP.Simple         (setRequestHost, setRequestPort)
import           URI.ByteString              (Absolute, URIRef, authorityHost,
                                              authorityPort, hostBS, portNumber,
                                              uriAuthority)
import           Web.Nest.HttpClient.Base    (Host)
import           Web.Nest.HttpClient.Request (NestRequest, NestResponse,
                                              RedirectOrResponse (..),
                                              sendRequest)

data RedirectState = RedirectState
  { host :: Host
  , port :: Int
  } deriving (Eq, Show)

type RedirectStateRef = IORef RedirectState

type Request a = RedirectStateRef -> NestRequest a -> IO (NestResponse a)

request :: (FromJSON a) => Request a
request hostRef req = do
  redirectState <- liftIO (readIORef hostRef)
  let redirectedReq = setHostAndPort redirectState req
  r <- liftIO (sendRequest redirectedReq)
  case r of
    Response x -> return x
    -- lol just keep redirecting forever
    Redirect uri -> do
      let hostAndPort = getHostAndPort uri
      writeIORef hostRef hostAndPort
      print ("Redirecting to: " <> (show hostAndPort))
      liftIO (request hostRef req)

setHostAndPort :: RedirectState -> NestRequest a -> NestRequest a
setHostAndPort RedirectState {host = h, port = p} req =
  setRequestHost h $ setRequestPort p req

getHostAndPort :: URIRef Absolute -> RedirectState
getHostAndPort uriRef =
  case uriAuthority uriRef of
    Nothing -> error "No URI authority"
    Just authority ->
      case authorityPort authority of
        Nothing -> error "No port"
        Just port ->
          RedirectState (hostBS (authorityHost authority)) (portNumber port)
