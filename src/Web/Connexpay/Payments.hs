{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module Web.Connexpay.Payments ( CreditCard(..)
                              , AuthResponse(..)
                              , authorisePayment
                              ) where

import Control.Monad.Reader (asks)
import Control.Monad.Writer.Strict
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Money
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.TypeError as TypeError
import Network.HTTP.Req

import Web.Connexpay.Data
import Web.Connexpay.Types

-- | Credit card info
--   No 'Show' instance should be made for this type
--   in order to avoid sensitive data leaks.
data CreditCard = CreditCard { number :: Text
                             , cardholder :: Maybe Text
                             , expiration :: (Word, Word) -- ^ Expiration date (month,year)
                             , cvv :: Maybe Text
                             }

type ShowError = TypeError.Text "CreditCard must not be shown in order to avoid leaking sensitive data"

instance TypeError ShowError => Show CreditCard where
  show = error "UNREACHABLE"

instance ToJSON CreditCard where
  toJSON cc = object
            $ execWriter
            $ do tell ["CardNumber" .= cc.number]
                 whenJust cc.cardholder $ \name ->
                   tell ["CardHolderName" .= name]
                 whenJust cc.cvv $ \cvv ->
                   tell ["Cvv2" .= cvv]
                 let expDate = padDate (tshow (snd cc.expiration)) <> padDate (tshow (fst cc.expiration))
                 tell ["ExpirationDate" .= expDate]

padDate :: Text -> Text
padDate t | Text.length t == 1 = "0" <> t
          | otherwise = t

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just x) f = f x
whenJust Nothing _ = pure ()

tshow :: Show a => a -> Text
tshow = Text.pack . show

data AuthResponse = AuthResponse { paymentGuid :: Text
                                 , status :: TransactionStatus
                                 , processorStatusCode :: Maybe Text
                                 , processorMessage :: Maybe Text
                                 } deriving (Show)

instance FromJSON AuthResponse where
  parseJSON (Object o) = AuthResponse <$> o .: "guid"
                                      <*> o .: "status"
                                      <*> o .:? "processorStatusCode"
                                      <*> o .:? "processorResponseMessage"
  parseJSON v = typeMismatch "AuthReponse" v

authorisePayment :: CreditCard -> Money USD -> ConnexpayM AuthResponse
authorisePayment cc amt = do guid <- asks (.deviceGuid)
                             tok <- bearerToken
                             host <- asks (.url)
                             let body = ReqBodyJson (reqBody guid)
                                 auth = header "Authorization" ("Bearer " <> Text.encodeUtf8 tok)
                                 url = https host /: "api" /: "v1" /: "authonlys"
                             resp <- req POST url body jsonResponse auth
                             pure (responseBody resp)
  where reqBody guid = object [ "Card" .= cc
                              , "Amount" .= getAmount amt
                              , "DeviceGuid" .= show guid
                              ]
