{-# LANGUAGE NamedFieldPuns #-}

module Web.Nest.HttpClient.RequestState
  ( request
  , HostStateRef
  ) where

import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson                  (FromJSON)

import           Data.IORef                  (IORef, readIORef, writeIORef)
import           Web.Nest.HttpClient.Base    (Host)
import           Web.Nest.HttpClient.Request (NestRequest, NestResponse,
                                              RedirectOrResponse (..),
                                              sendRequest)

type HostStateRef = IORef Host

request :: (FromJSON a) => HostStateRef -> NestRequest a -> IO (NestResponse a)
request hostRef req = do
  host <- liftIO (readIORef hostRef)
  r <- liftIO (sendRequest host req)
  case r of
    Response x -> return x
    Redirect h -> do
      writeIORef hostRef h
      liftIO (request hostRef req)
