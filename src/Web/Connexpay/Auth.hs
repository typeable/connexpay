{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Connexpay.Auth (authenticate) where

import Web.Connexpay.Types

import Control.Monad
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Network.HTTP.Req
import Numeric.Natural
import Web.FormUrlEncoded
import Web.HttpApiData

data AuthForm = AuthForm { login :: Text
                         , password :: Text
                         }

instance ToForm AuthForm where
  toForm auth = [ ("grant_type", toQueryParam ("password" :: Text))
                , ("username", toQueryParam auth.login)
                , ("password", toQueryParam auth.password)
                ]

mkAuthForm :: Text -> Text -> ByteString
mkAuthForm login passwd = ByteString.toStrict (urlEncodeAsForm form)
  where form = AuthForm login passwd

data TokenReply = TokenReply { token :: BearerToken
                             , expires_in :: Natural
                             } deriving (Show)

instance FromJSON TokenReply where
  parseJSON (Object v) = do typ <- v .: "token_type"
                            unless (typ == "bearer") $
                              fail ("Unsupported token type returned: " <> Text.unpack typ)
                            TokenReply <$> v .: "access_token"
                                       <*> v .: "expires_in"
  parseJSON v = typeMismatch "TokenReply" v

authenticate :: ConnexpayM (BearerToken, Natural)
authenticate = do login <- asks (.login)
                  password <- asks (.password)
                  host <- asks (.url)
                  let body = ReqBodyBs (mkAuthForm login password)
                      url = https host /: "api" /: "v1" /: "token"
                  resp <- req POST url body jsonResponse mempty
                  let TokenReply tok ts = responseBody resp
                  pure (tok, ts)
