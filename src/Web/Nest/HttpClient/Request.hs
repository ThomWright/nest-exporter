{-# LANGUAGE NamedFieldPuns   #-}

module Web.Nest.HttpClient.Request
  ( sendRequest
  , NestRequest
  ) where

import           Data.Aeson                (FromJSON, eitherDecode')
import           Data.Text                 (pack)
import           Network.HTTP.Client       (Request)
import           Network.HTTP.Simple       (getResponseBody,
                                            getResponseStatusCode, httpLBS)
import           Web.Nest.HttpClient.Error (NestApiError (..), NestError (..))

type NestRequest a = Request

sendRequest ::
     (FromJSON a)
  => NestRequest a
  -> IO (Either NestError a)
sendRequest req = do
  response <- httpLBS req
  let status = getResponseStatusCode response
  let body = getResponseBody response
  return
    (case status of
       200 ->
         case eitherDecode' body of
           (Left message) -> Left (ParseFailure (pack message))
           (Right a)      -> (Right a)
       s
         | s >= 400 ->
           case eitherDecode' body of
             (Left message) -> Left (ParseFailure (pack message))
             (Right a)      -> Left (ApiError (NestApiError status a))
       _ -> Left (UnknownStatusCode status))
