{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module Web.Connexpay.Payments ( CreditCard(..)
                              , AuthResponse(..)
                              , authorisePayment
                              , VoidRequest(..)
                              , voidPayment
                              , CaptureResponse(..)
                              , capturePayment
                              , cancelPayment
                              , returnPayment
                              ) where

import Control.Monad (when,void)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Control.Monad.Writer.Strict
import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair, typeMismatch)
import Data.ByteString.Lazy qualified as ByteString
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
  do mtok <- bearerToken
     tok <- case mtok of
       Just t -> pure t
       Nothing -> throwError (ConnectionError $ TokenError "No authentication token available. Check connection parameters?")
     host <- asks (.url)
     tls <- asks (.useTLS)
     let auth = header "Authorization" ("Bearer " <> Text.encodeUtf8 tok)
         url s = s host /: "api" /: "v1" /: endpoint
     obj <- object <$> addGuid body
     let jbody = ReqBodyJson obj
     r <- if tls
       then reqCb POST (url https) jbody resp auth (logRequest obj)
       else reqCb POST (url http) jbody resp auth (logRequest obj)
     pure r
  where
    addGuid b =
      do guid <- asks (.deviceGuid)
         return (b <> [ "DeviceGuid" .= show guid ])
    logRequest v r =
      do log_ <- asks (.logAction)
         -- Remove card info from logs
         let v' = case v of
                    Object obj
                      | Just c <- KeyMap.lookup "Card" obj -> Object (KeyMap.insert "Card" (replaceCard c) obj)
                    other -> other
             msg = Text.unlines [ "Connexpay request:"
                                , tshow r
                                , Text.decodeUtf8 (ByteString.toStrict $ encode v')
                                ]
         _ <- liftIO (log_ msg)
         return r
    replaceCard (Object c) =
      Object
        $ KeyMap.insert "CardNumber" (String "<REDACTED>")
        $ KeyMap.insert "Cvv2" (String "<REDACTED>")
        $ c
    replaceCard v = v

sendRequestJson :: FromJSON a => Text -> [Pair] -> ConnexpayM (JsonResponse a)
sendRequestJson = sendRequest' jsonResponse

sendRequest_ :: Text -> [Pair] -> ConnexpayM ()
sendRequest_ ep = void . sendRequest' ignoreResponse ep

data AuthResponse = AuthResponse { paymentGuid :: AuthOnlyGuid
                                 , status :: TransactionStatus
                                 , processorStatusCode :: Maybe Text
                                 , processorMessage :: Maybe Text
                                 , addressVerificationCode :: Maybe Text
                                 , cvvVerificationCode :: Maybe Text
                                 } deriving (Show)

instance FromJSON AuthResponse where
  parseJSON (Object o) = AuthResponse <$> o .: "guid"
                                      <*> o .: "status"
                                      <*> o .:? "processorStatusCode"
                                      <*> o .:? "processorResponseMessage"
                                      <*> o .:? "addressVerificationCode"
                                      <*> o .:? "cvvVerificationCode"
  parseJSON v = typeMismatch "AuthReponse" v

-- | Authorise a credit card payment.
authorisePayment :: CreditCard -- ^ Credit card details (see 'CreditCard')
                 -> Money USD  -- ^ Amount to charge, USD
                 -> Maybe Text -- ^ Invoice description
                 -> Maybe Text -- ^ Merchant description that will appear in a customer's statement.
                 -> ConnexpayM AuthResponse
authorisePayment cc amt invoice vendor =
  do resp <- sendRequestJson "authonlys" body
     let rbody = responseBody resp
     -- Special case for Connexpay local transaction
     -- This status means that the transaction was registered,
     -- but Connexpay stopped its processing and it won't be
     -- moved any further.
     -- Also, when I asked Ken from Connexpay about this,
     -- he told me he had never seen this status before.
     when (rbody.status == TransactionCreatedLocal) $
       throwError (PaymentFailure LocalTransaction Nothing)
     pure rbody
  where body = execWriter $
                do tell [ "Card" .= cc ]
                   tell [ "Amount" .= getAmount amt ]
                   whenJust invoice $ \i ->
                     tell [ "OrderNumber" .= i ]
                   whenJust vendor $ \v ->
                     tell [ "StatementDescription" .= v ]
                   -- We are supposed to pass RiskData, but it still
                   -- can be an empty object. Consider population this
                   -- should the need arise.
                   tell [ "RiskData" .= KeyMap.empty @() ]

data VoidRequest = VoidAuthorized AuthOnlyGuid | VoidCaptured SaleGuid (Maybe (Money USD))

-- | Void payment
voidPayment :: VoidRequest
            -> ConnexpayM ()
voidPayment (VoidAuthorized pid) = sendRequest_ "void" body
  where body = [ "AuthOnlyGuid" .= show pid ]
voidPayment (VoidCaptured pid amt) = sendRequest_ "void" body
  where body = execWriter $
          do tell [ "SaleGuid" .= show pid ]
             whenJust amt $ \m ->
               tell [ "Amount" .= getAmount m ]

-- | Internal data type for Capture requests.
data CPTransaction = CPTransaction { expectedPayments :: Int32 }

instance ToJSON CPTransaction where
  toJSON t = object [ "ExpectedPayments" .= t.expectedPayments ]

-- | Response for the payment capture request
data CaptureResponse = CaptureResponse { captureGuid :: CaptureGuid
                                       , saleGuid :: SaleGuid
                                       , saleStatus :: TransactionStatus
                                       } deriving (Show)

instance FromJSON CaptureResponse where
  parseJSON (Object o) =
    do cguid <- o .: "guid"
       sale <- o .: "sale"
       saleGuid <- sale .: "guid"
       status <- sale .: "status"
       pure (CaptureResponse cguid saleGuid status)
  parseJSON v = typeMismatch "CaptureResponse" v

-- | Capture payment, previously authorised through 'authorisePayment'.
capturePayment :: SaleGuid  -- ^ Sales GUID, obtained from 'authorisePayment'.
               -> ConnexpayM CaptureResponse
capturePayment pid =
  do resp <- sendRequestJson "Captures" body
     let rbody = responseBody resp
     pure rbody
  where body = [ "AuthOnlyGuid" .= show pid
               , "ConnexPayTransaction" .= CPTransaction 1 ]

-- | Cancel voided or captured payment.
--   In case of an authorised-only payment, voiding is performed.
--   Otherwise, a payment goes through a refund process.
cancelPayment :: SaleGuid -- ^ Sales GUID, obtained from 'capturePayment'.
              -> ConnexpayM ()
cancelPayment pid = sendRequest_ "cancel" body
  where body = [ "SaleGuid" .= show pid ]

returnPayment :: SaleGuid -- ^ Sales GUID, obtained from 'capturePayment'.
              -> Maybe (Money USD)
              -> ConnexpayM ()
returnPayment pid amt = sendRequest_ "returns" body
  where body = execWriter $
          do tell [ "SaleGuid" .= show pid ]
             whenJust amt $ \m ->
               tell [ "Amount" .= getAmount m ]
