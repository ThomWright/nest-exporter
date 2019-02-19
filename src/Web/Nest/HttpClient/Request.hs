{-# LANGUAGE NamedFieldPuns #-}

module Web.Nest.HttpClient.Request
  ( sendRequest
  , NestRequest
  , NestResponse
  , RedirectOrResponse(..)
  ) where

import           Data.Aeson                (FromJSON, eitherDecode')
import           Data.Text                 (pack)

-- import qualified Data.ByteString.Char8       as S8
import qualified Data.ByteString.Lazy      as LBS
import           Network.HTTP.Client       (Request, Response, responseHeaders)
import           Network.HTTP.Types.Header (hLocation)

-- import           Network.HTTP.Client       (Request)
import           Network.HTTP.Simple       (getResponseBody,
                                            getResponseStatusCode, httpLBS,
                                            setRequestHost)
import           Web.Nest.HttpClient.Base  (Host)
import           Web.Nest.HttpClient.Error (NestApiError (..), NestError (..))

type NestRequest a = Request

type NestResponse a = (Either NestError a)

data RedirectOrResponse a
  = Redirect Host
  | Response (NestResponse a)

sendRequest ::
     (FromJSON a) => Host -> NestRequest a -> IO (RedirectOrResponse a)
sendRequest host req = do
  let reqreq = setRequestHost host req
  response <- httpLBS reqreq
  let status = getResponseStatusCode response
  let body = getResponseBody response
  return
    (case status
      --  200 ->
      --    case eitherDecode' body of
      --      (Left message) -> Left (ParseFailure (pack message))
      --      (Right a)      -> (Right a)
      --  s
      --    | s >= 400 ->
      --      case eitherDecode' body of
      --        (Left message) -> Left (ParseFailure (pack message))
      --        (Right a)      -> Left (ApiError (NestApiError status a))
      --  _ -> Left (UnknownStatusCode status))
           of
       200 -> handleSuccess body
       301 -> handle301 response
       s
         | s >= 400 -> handleError status body
       _ -> Response $ Left (UnknownStatusCode status))

handleSuccess :: (FromJSON a) => LBS.ByteString -> RedirectOrResponse a
handleSuccess body =
  Response $
  case eitherDecode' body of
    (Left message) -> Left (ParseFailure (pack message))
    (Right a)      -> (Right a)

handleError :: Int -> LBS.ByteString -> RedirectOrResponse a
handleError status body =
  Response $
  case eitherDecode' body of
    (Left message) -> Left (ParseFailure (pack message))
    (Right a)      -> Left (ApiError (NestApiError status a))

handle301 :: Response b -> RedirectOrResponse a
handle301 response =
  case (lookup hLocation) (responseHeaders response) of
    Just l -> Redirect l
    Nothing ->
      Response $ Left (UnknownStatusCode (getResponseStatusCode response))
