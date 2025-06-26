{-# LANGUAGE OverloadedLists #-}

module Web.Connexpay.Auth
  ( AuthReply(..)
  , TokenReply(..)
  , authenticate
  ) where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as Lazy
import Data.ByteString.Lazy qualified as Lazy.ByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types
import Numeric.Natural
import Web.FormUrlEncoded
import Web.HttpApiData

import Web.Connexpay.Http
import Web.Connexpay.Types


data AuthForm = AuthForm { login :: Text
                         , password :: Text
                         }

instance ToForm AuthForm where
  toForm auth = [ ("grant_type", toQueryParam ("password" :: Text))
                , ("username", toQueryParam auth.login)
                , ("password", toQueryParam auth.password)
                ]

mkAuthForm :: Config -> ByteString
mkAuthForm cfg = ByteString.toStrict (urlEncodeAsForm form)
  where form = AuthForm cfg.login cfg.password

data AuthReply
  = Authorized TokenReply
  | Unauthorized (HTTP.Response Lazy.ByteString)
  | AuthParseError String
  deriving stock (Show)

data TokenReply = TokenReply { token :: BearerToken
                             , expiresIn :: Natural
                             } deriving stock (Show)

instance FromJSON TokenReply where
  parseJSON (Object v) = do typ <- v .: "token_type"
                            unless (typ == "bearer") $
                              fail ("Unsupported token type returned: " <> Text.unpack typ)
                            TokenReply <$> v .: "access_token"
                                       <*> v .: "expires_in"
  parseJSON v = typeMismatch "TokenReply" v

authenticate :: Config -> Env -> IO AuthReply
authenticate config env = do
  let
    req = HTTP.defaultRequest
      { HTTP.method = "POST"
      , HTTP.host = Text.encodeUtf8 config.host
      , HTTP.port = if config.useTLS then 443 else 80
      , HTTP.secure = config.useTLS
      , HTTP.path = "api/v1/token"
      , HTTP.requestHeaders =
        [ ("Accept", "application/json")
        , ("Accept-Encoding", "gzip")
        ]
      , HTTP.requestBody = HTTP.RequestBodyBS $ mkAuthForm config
      }
  env.logAction $ httpLog req $ "request" .= show @HTTP.Request req
  resp <- HTTP.httpLbs req env.manager
  let
    res
      | statusIsSuccessful resp.responseStatus
      = either AuthParseError Authorized $ eitherDecode resp.responseBody
      | otherwise
      = Unauthorized resp
  env.logAction $ httpLog req case res of
    -- Don't log body! As it contains sensitive info
    Authorized _ -> "result".= ("success" :: Text)
    Unauthorized _ -> "result" .= ("unauthorized" :: Text) <> "body:" .=
      (Text.decodeUtf8Lenient $ Lazy.ByteString.toStrict resp.responseBody)
    AuthParseError err -> mconcat
      [ "result" .= ("parse_error: " <> err)
      , "body" .=
        (Text.decodeUtf8Lenient $ Lazy.ByteString.toStrict resp.responseBody)
      ]
  pure res
