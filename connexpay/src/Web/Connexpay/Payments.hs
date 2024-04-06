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
data CreditCard = CreditCard { number :: Text             -- ^ Credit card number, as 'Text'.
                             , cardholder :: Maybe Text   -- ^ Cardholder name, optional.
                             , expiration :: (Word, Word) -- ^ Expiration date (month,year).
                             , cvv :: Maybe Text          -- ^ CVC/CVV code.
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
     tls <- asks (.useTLS)
     let auth = header "Authorization" ("Bearer " <> Text.encodeUtf8 tok)
         url s = s host /: "api" /: "v1" /: endpoint
     jbody <- ReqBodyJson . object <$> addGuid body
     if tls
       then req POST (url https) jbody resp auth
       else req POST (url http) jbody resp auth
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

-- | Authorise a credit card payment.
authorisePayment :: CreditCard -- ^ Credit card details (see 'CreditCard')
                 -> Money USD  -- ^ Amount to charge, USD
                 -> Maybe Text -- ^ Merchant description that will appear in a customer's statement.
                 -> ConnexpayM AuthResponse
authorisePayment cc amt vendor = do resp <- sendRequestJson "authonlys" body
                                    pure (responseBody resp)
  where body = execWriter $
                do tell [ "Card" .= cc ]
                   tell ["Amount" .= getAmount amt ]
                   whenJust vendor $ \v ->
                     tell [ "StatementDescription" .= v ]

-- | Void payment
voidPayment :: SaleGuid           -- ^ Sales GUID, obtained from 'authorisePayment'.
            -> Maybe (Money USD)  -- ^ Optionally, you may only void a partial sum, with the rest being subsequently charged.
            -> ConnexpayM ()
voidPayment pid amt = sendRequest_ "void" body
  where body = execWriter $
          do tell [ "AuthOnlyGuid" .= show pid ]
             whenJust amt $ \m ->
               tell [ "Amount" .= getAmount m ]

-- | Internal data type for Capture requests.
data CPTransaction = CPTransaction { expectedPayments :: Int32 }

instance ToJSON CPTransaction where
  toJSON t = object [ "ExpectedPayments" .= t.expectedPayments ]

-- | Capture payment, previously authorised through 'authorisePayment'.
capturePayment :: SaleGuid  -- ^ Sales GUID, obtained from 'authorisePayment'.
               -> ConnexpayM ()
capturePayment pid = sendRequest_ "Captures" body
  where body = [ "AuthOnlyGuid" .= show pid
               , "ConnexPayTransaction" .= CPTransaction 1 ]

-- | Cancel voided or captured payment.
--   In case of an authorised-only payment, voiding is performed.
--   Otherwise, a payment goes through a refund process.
cancelPayment :: SaleGuid -- ^ Sales GUID, obtained from 'authorisePayment'.
              -> ConnexpayM ()
cancelPayment pid = sendRequest_ "cancel" body
  where body = [ "SaleGuid" .= show pid ]