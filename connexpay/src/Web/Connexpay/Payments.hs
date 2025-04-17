{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Connexpay.Payments ( CreditCard(..)
                              , AuthRequest(..)
                              , Customer(..)
                              , AuthResponse(..)
                              , authorisePayment
                              , VoidRequest(..)
                              , voidPayment
                              , CaptureResponse(..)
                              , capturePayment
                              , cancelPayment
                              , returnPayment
                              ) where

import Control.Monad.Writer.Strict
import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.TH
import Data.Aeson.Types (typeMismatch)
import Data.Functor
import Data.Int (Int32)
import Data.Maybe
import Data.Money
import Data.Text (Text)
import Data.Text qualified as Text
import Data.UUID (UUID)
import GHC.TypeError as TypeError

import Web.Connexpay.Http
import Web.Connexpay.Types
import Web.Connexpay.Utils


data Customer = Customer { address1 :: Text
                         , address2 :: Maybe Text
                         , zip :: Maybe Text
                         }

deriveToJSON aesonOptions ''Customer

maskCustomer :: LogMasker Customer
maskCustomer customer = Customer
  { address1 = redactWords customer.address1
  , address2 = redactWords <$> customer.address2
  , zip = customer.zip
  }

-- | Credit card info
--   No 'Show' instance should be made for this type
--   in order to avoid sensitive data leaks.
data CreditCard = CreditCard { number :: Text             -- ^ Credit card number, as 'Text'.
                             , cardholder :: Maybe Text   -- ^ Cardholder name, optional.
                             , expiration :: (Word, Word) -- ^ Expiration date (month,year).
                             , cvv :: Maybe Text          -- ^ CVC/CVV code.
                             , customer :: Maybe Customer -- ^ Required to get AVS
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
                 whenJust cc.customer $ \customer ->
                   tell [ "Customer" .= customer ]

padDate :: Text -> Text
padDate t | Text.length t == 1 = "0" <> t
          | otherwise = t

maskCreditCard :: LogMasker CreditCard
maskCreditCard cc = CreditCard
  { number = maskCreditCardNumber cc.number
  , cardholder = redactWords <$> cc.cardholder
  , expiration = (0, 0)
  , cvv = redactWords <$> cc.cvv
  , customer = maskCustomer <$> cc.customer
  }

maskCreditCardNumber :: Text -> Text
maskCreditCardNumber number = firstDigits <> stars <> lastDigits
  where
    (firstDigits, rest) = Text.splitAt 4 number
    toDrop = Text.length rest - 4
    (_, lastDigits) = Text.splitAt toDrop rest
    stars = Text.replicate toDrop "*"

data AuthResponse = AuthResponse { paymentGuid :: AuthOnlyGuid
                                 , status :: TransactionStatus
                                 , processorStatusCode :: Maybe Text
                                 , processorMessage :: Maybe Text
                                 , addressVerificationCode :: Maybe Text
                                 , cvvVerificationCode :: Maybe Text
                                 } deriving stock (Show)

instance FromJSON AuthResponse where
  parseJSON (Object o) = AuthResponse <$> o .: "guid"
                                      <*> o .: "status"
                                      <*> o .:? "processorStatusCode"
                                      <*> o .:? "processorResponseMessage"
                                      <*> o .:? "addressVerificationCode"
                                      <*> o .:? "cvvVerificationCode"
  parseJSON v = typeMismatch "AuthReponse" v

-- | Transaction status in Connexpay
-- The list is taken from https://docs.connexpay.com/reference/search-sales
data TransactionStatus
  = TransactionApproved
  | TransactionDeclined
  | TransactionCreatedLocal
    -- ^ Seems to only exist in a test environment, indicates success.
  | TransactionCreatedProcNotReached
    -- ^ Communication error between Connexpay and Card Processor
  | TransactionCreatedProcError
    -- ^ Processor errored out
  | TransactionApprovedWarning
    -- ^ Wut 0__o FIXME: figure out what this is
  | TransactionOther Text
    -- ^ In case they return something unexpected
  deriving stock (Eq, Ord, Show)

instance FromJSON TransactionStatus where
  parseJSON = withText "TransactionStatus" $ pure . \case
    "Transaction - Approved" ->
      TransactionApproved
    "Transaction - Declined" ->
      TransactionDeclined
    "Transaction - CreatedLocal" ->
      TransactionCreatedLocal
    "Transaction - Created - Error: Processor not reached" ->
      TransactionCreatedProcNotReached
    "Transaction - Processor Error" ->
      TransactionCreatedProcError
    "Transaction - Approved - Warning" ->
      TransactionApprovedWarning
    other ->
      TransactionOther other

data AuthRequest = AuthRequest { creditCard :: CreditCard
                               , amount :: Money USD
                               , invoice :: Maybe Text
                               , vendor :: Maybe Text
                                 -- ^ Merchant description that will appear in a
                                 -- customer's statement.
                               }

instance ToJSON AuthRequest where
  toJSON request = object $ catMaybes
    [ Just $ "Card" .= request.creditCard
    , Just $  "Amount" .= getAmount request.amount
    , request.invoice <&> \i -> "OrderNumber" .= i
    , request.vendor <&> \v -> "StatementDescription" .= v
    -- We are supposed to pass RiskData, but it still
    -- can be an empty object. Consider population this
    -- should the need arise.
    , Just $ "RiskData" .= KeyMap.empty @()
    ]

maskAuthRequest :: LogMasker AuthRequest
maskAuthRequest req = AuthRequest
  { creditCard = maskCreditCard req.creditCard
  , amount = req.amount
  , invoice = req.invoice
  , vendor = req.vendor
  }

data VoidRequest
  = VoidAuthorized AuthOnlyGuid
  | VoidCaptured SaleGuid (Maybe (Money USD))

instance ToJSON VoidRequest where
  toJSON = \case
    VoidAuthorized pid -> object ["AuthOnlyGuid" .= show @UUID pid]
    VoidCaptured pid mbAmount -> object $ catMaybes
      [ Just $ "SaleGuid" .= show pid
      , mbAmount <&> \amount -> "Amount" .= getAmount amount
      ]

-- | Authorise a credit card payment.
authorisePayment :: Connexpay -> Env -> AuthRequest -> IO (Response AuthResponse)
authorisePayment connexpay env raw = doRequest connexpay env "authonlys" RequestBody
  { raw
  , logMasker = maskAuthRequest
  }

-- | Void payment
voidPayment :: Connexpay -> Env -> VoidRequest -> IO (Response ())
voidPayment connexpay env raw = doRequest_ connexpay env "void" RequestBody
  { raw
  , logMasker = id
  }

-- | Internal data type for Capture requests.
data CPTransaction = CPTransaction { expectedPayments :: Int32 }

instance ToJSON CPTransaction where
  toJSON t = object [ "ExpectedPayments" .= t.expectedPayments ]

-- | Response for the payment capture request
data CaptureResponse = CaptureResponse { captureGuid :: CaptureGuid
                                       , saleGuid :: SaleGuid
                                       , saleStatus :: TransactionStatus
                                       } deriving stock (Show)

instance FromJSON CaptureResponse where
  parseJSON (Object o) =
    do cguid <- o .: "guid"
       sale <- o .: "sale"
       saleGuid <- sale .: "guid"
       status <- sale .: "status"
       pure (CaptureResponse cguid saleGuid status)
  parseJSON v = typeMismatch "CaptureResponse" v

-- | Capture payment, previously authorised through 'authorisePayment'.
capturePayment :: Connexpay
               -> Env
               -> SaleGuid  -- ^ Sales GUID, obtained from 'authorisePayment'.
               -> IO (Response CaptureResponse)
capturePayment connexpay env pid = doRequest connexpay env "Captures" RequestBody
  { raw = object
    [ "AuthOnlyGuid" .= show @UUID pid
    , "ConnexPayTransaction" .= CPTransaction 1
    ]
  , logMasker = id
  }

-- | Cancel voided or captured payment.
--   In case of an authorised-only payment, voiding is performed.
--   Otherwise, a payment goes through a refund process.
cancelPayment :: Connexpay
              -> Env
              -> SaleGuid -- ^ Sales GUID, obtained from 'capturePayment'.
              -> IO (Response ())
cancelPayment connexpay env pid = doRequest_ connexpay env "cancel" RequestBody
  { raw = object [ "SaleGuid" .= show pid ]
  , logMasker = id
  }

returnPayment :: Connexpay
              -> Env
              -> SaleGuid -- ^ Sales GUID, obtained from 'capturePayment'.
              -> Maybe (Money USD)
              -> IO (Response ())
returnPayment connexpay env pid amt = doRequest_ connexpay env "returns" RequestBody
  { raw = object $ catMaybes
    [ Just $ "SaleGuid" .= show @UUID pid
    , amt <&> \m -> "Amount" .= getAmount m
    ]
  , logMasker = id
  }
