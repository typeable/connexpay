module Web.Connexpay.Http
  ( RequestBody(..)
  , LogMasker
  , doRequest
  , doRequest_
  , fromResponse
  , httpLog
  ) where

import Control.Concurrent
import Control.Exception.Safe
import Data.Aeson
import Data.Aeson.Text
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.ByteString.Lazy qualified as Lazy.ByteString
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as Lazy.Text
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types

import Web.Connexpay.Types


data RequestBody a = RequestBody
  { raw :: a
    -- ^ To be sent to Connexpay
  , logMasker :: LogMasker a
  }

type LogMasker a = a -> a

doRequest
  :: (ToJSON req, FromJSON resp)
  => Connexpay
  -> Env
  -> Text
  -> RequestBody req
  -> IO (Response () resp)
  -- ^ Left on non-200 repsonses
doRequest connexpay env endpoint body = readMVar connexpay.bearerToken >>= \case
  Nothing -> pure MissingOrExpiredToken
  Just (BearerToken token) -> do
    let
      req = HTTP.applyBearerAuth (Text.encodeUtf8 token) HTTP.defaultRequest
        { HTTP.method = "POST"
        , HTTP.host = Text.encodeUtf8 connexpay.config.host
        , HTTP.port = if connexpay.config.useTLS then 443 else 80
        , HTTP.secure = connexpay.config.useTLS
        , HTTP.path = "api/v1/" <> Text.encodeUtf8 endpoint
        , HTTP.requestHeaders =
          [ ("Content-Type", "application/json; charset=utf-8")
          , ("Accept", "application/json")
          , ("Accept-Encoding", "gzip")
          ]
        , HTTP.requestBody = HTTP.RequestBodyLBS $ encode
          case toJSON body.raw of
            Object o -> Object $ o <> "DeviceGuid" .= connexpay.config.deviceGuid
            v -> v
        }
    env.logAction $ httpLog req $ mconcat
      [ "request" .= show @HTTP.Request req
      , "body" .= body.logMasker body.raw
      ]
    resp <- HTTP.httpLbs req env.manager
    env.logAction $ httpLog req $ mconcat
      [ "response_code" .= statusCode resp.responseStatus
      , "body" .= Text.decodeUtf8Lenient (Lazy.ByteString.toStrict resp.responseBody)
      ]
    either throwM pure $ fromResponse resp

doRequest_
  :: (ToJSON req)
  => Connexpay
  -> Env
  -> Text
  -> RequestBody req
  -> IO (Response () ())
doRequest_ connexpay env endpoint body = fmap @(Response ()) (const @_ @Value ())
  <$> doRequest connexpay env endpoint body

-- | We should aim to never see this in practice.
data FromResponseException
  = UnexpectedResponse (HTTP.Response Lazy.ByteString)
    -- ^ Unexpected response status code (neither 2XX, nor 422)
  | ResponseSuccessParseError (HTTP.Response Lazy.ByteString) String
    -- ^ Failed to parse /success/ response.
    -- Thus, operation is likely successful, but we can't analyze the response.
  | ResponseErrorParseError (HTTP.Response Lazy.ByteString) String
    -- ^ Failed to parse /error/ (422) response.
  deriving stock (Show)
  deriving anyclass (Exception)

fromResponse
  :: (FromJSON a)
  => HTTP.Response Lazy.ByteString
  -> Either FromResponseException (Response () a)
fromResponse resp
  | statusIsSuccessful resp.responseStatus
  = either (Left . ResponseSuccessParseError resp) (Right . ResponseSuccess) $
    eitherDecode resp.responseBody
  | resp.responseStatus == unauthorized401
  = pure MissingOrExpiredToken
  | resp.responseStatus == unprocessableEntity422
  = either (Left . ResponseErrorParseError resp) (Right . ResponseError) $
    eitherDecode resp.responseBody
  | otherwise
  = Left $ UnexpectedResponse resp

httpLog :: HTTP.Request -> Object -> Text
httpLog req payload = Lazy.Text.toStrict $ encodeToLazyText @Object $
  "connexpay" .= (payload <> "path" .= Text.decodeUtf8Lenient req.path)
