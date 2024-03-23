{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module Web.Connexpay.Payments ( CreditCard(..)
                              , AuthResponse(..)
                              , authorisePayment
                              , voidPayment
                              , capturePayment
                              , cancelPayment
                              ) where

import Control.Monad (void)
import Control.Monad.Reader (asks)
import Control.Monad.Writer.Strict
import Data.Aeson
import Data.Aeson.Types (Pair, typeMismatch)
import Data.Int (Int32)
import Data.Money
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.TypeError as TypeError
import Network.HTTP.Req

import Web.Connexpay.Data
import Web.Connexpay.Types
import Web.Connexpay.Utils

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

sendRequest' :: HttpResponse resp => Proxy resp -> Text -> [Pair] -> ConnexpayM resp
sendRequest' resp endpoint body =
  do tok <- bearerToken
     host <- asks (.url)
     let auth = header "Authorization" ("Bearer " <> Text.encodeUtf8 tok)
         url = https host /: "api" /: "v1" /: endpoint
     jbody <- ReqBodyJson . object <$> addGuid body
     req POST url jbody resp auth
  where addGuid b = do guid <- asks (.deviceGuid)
                       return (b <> [ "DeviceGuid" .= show guid ])

sendRequestJson :: FromJSON a => Text -> [Pair] -> ConnexpayM (JsonResponse a)
sendRequestJson = sendRequest' jsonResponse

sendRequest_ :: Text -> [Pair] -> ConnexpayM ()
sendRequest_ ep = void . sendRequest' ignoreResponse ep

data AuthResponse = AuthResponse { paymentGuid :: SaleGuid
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
authorisePayment cc amt = do resp <- sendRequestJson "authonlys" body
                             pure (responseBody resp)
  where body = [ "Card" .= cc
               , "Amount" .= getAmount amt
               ]

voidPayment :: SaleGuid -> Maybe (Money USD) -> ConnexpayM ()
voidPayment pid amt = sendRequest_ "void" body
  where body = execWriter $
          do tell [ "AuthOnlyGuid" .= show pid ]
             whenJust amt $ \m ->
               tell [ "Amount" .= getAmount m ]

-- | Internal data type for Capture requests.
data CPTransaction = CPTransaction { expectedPayments :: Int32 }

instance ToJSON CPTransaction where
  toJSON t = object [ "ExpectedPayments" .= t.expectedPayments ]


capturePayment :: SaleGuid -> ConnexpayM ()
capturePayment pid = sendRequest_ "Captures" body
  where body = [ "AuthOnlyGuid" .= show pid
               , "ConnexPayTransaction" .= CPTransaction 1 ]

cancelPayment :: SaleGuid -> ConnexpayM ()
cancelPayment pid = sendRequest_ "cancel" body
  where body = [ "SaleGuid" .= show pid ]
