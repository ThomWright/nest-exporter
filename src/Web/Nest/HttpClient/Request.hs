{-# LANGUAGE NamedFieldPuns #-}

module Web.Nest.HttpClient.Request
  ( sendRequest
  , NestRequest
  , NestResponse
  , RedirectOrResponse(..)
  ) where

import           Data.Aeson                (FromJSON, eitherDecode')
import qualified Data.ByteString.Char8     as S8
import qualified Data.ByteString.Lazy      as LBS
import           Data.Text                 (pack)
import           Network.HTTP.Client       (Request, Response, responseHeaders)
import           Network.HTTP.Simple       (getResponseBody,
                                            getResponseStatusCode, httpLBS)
import           Network.HTTP.Types.Header (hLocation)
import           URI.ByteString            (Absolute, URIRef, parseURI,
                                            strictURIParserOptions)
import           Web.Nest.HttpClient.Error (NestApiError (..), NestError (..))
import           Web.Nest.Text             (bs2t)

type NestRequest a = Request

type NestResponse a = (Either NestError a)

data RedirectOrResponse a
  = Redirect (URIRef Absolute)
  | Response (NestResponse a)

sendRequest :: (FromJSON a) => NestRequest a -> IO (RedirectOrResponse a)
sendRequest req = do
  response <- httpLBS req
  let status = getResponseStatusCode response
  let body = getResponseBody response
  return
    (case status of
       200 -> handleSuccess body
       s
         | s == 301 || s == 307 -> handleRedirect response
       s
         | s >= 400 -> handleError status body
       _ -> Response $ Left (UnknownStatusCode status))

handleSuccess :: (FromJSON a) => LBS.ByteString -> RedirectOrResponse a
handleSuccess body =
  Response $
  case eitherDecode' body of
    (Left message) -> Left (ParseFailure (pack message))
    (Right a)      -> Right a

handleError :: Int -> LBS.ByteString -> RedirectOrResponse a
handleError status body =
  Response $
  case eitherDecode' body of
    (Left message) -> Left (ParseFailure (pack message))
    (Right a)      -> Left (ApiError (NestApiError status a))

handleRedirect :: Response b -> RedirectOrResponse a
handleRedirect response =
  case lookup hLocation (responseHeaders response) of
    Just locationHeader ->
      case getHost locationHeader of
        Left err ->
          Response $
          Left
            (ParseFailure
               ("Failed to parse redirect location header: " <> bs2t err))
        Right uri -> Redirect uri
    Nothing ->
      Response $ Left (UnknownStatusCode (getResponseStatusCode response))

getHost :: S8.ByteString -> Either S8.ByteString (URIRef Absolute)
getHost bsUri =
  case parseURI strictURIParserOptions bsUri of
    Left parseError -> Left (S8.pack ("Unparseable URI - " <> show parseError))
    Right uriRef -> Right uriRef
