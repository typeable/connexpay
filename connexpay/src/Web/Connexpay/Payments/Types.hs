{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Connexpay.Payments.Types where

import Data.Aeson
import Data.Aeson.TH
import Data.Fixed
import Data.Int (Int32)
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.TypeError as TypeError
import Text.Printf

import Web.Connexpay.Http
import Web.Connexpay.Types
import Web.Connexpay.Utils


data AuthRequest = AuthRequest
  { card :: CreditCard
  , amount :: USD
  , orderNumber :: Maybe Text
  , statementDescription :: Maybe Text
    -- ^ Merchant description that will appear in a
    -- customer's statement.
  , riskData :: RiskData
  }

maskAuthRequest :: LogMasker AuthRequest
maskAuthRequest req = AuthRequest
  { card = maskCreditCard req.card
  , amount = req.amount
  , orderNumber = starWords <$> req.orderNumber
  , statementDescription = req.statementDescription
  , riskData = maskRiskData req.riskData
  }

newtype USD = USD Centi
  deriving newtype (Read, FromJSON, ToJSON)

-- | Credit card info
--   No 'Show' instance should be made for this type
--   in order to avoid sensitive data leaks.
data CreditCard = CreditCard
  { cardNumber :: Text
  , cardHolderName :: Maybe Text
  , expirationDate :: ExpirationDate
  , cvv2 :: Maybe Text
  , customer :: Maybe Customer
    -- ^ Required to get AVS
  }

type ShowError = TypeError.Text
  "CreditCard must not be shown in order to avoid leaking sensitive data"

instance TypeError ShowError => Show CreditCard where
  show = error "UNREACHABLE"

maskCreditCard :: LogMasker CreditCard
maskCreditCard cc = CreditCard
  { cardNumber = maskCreditCardNumber cc.cardNumber
  , cardHolderName = starWords <$> cc.cardHolderName
  , expirationDate = maskExpirationDate cc.expirationDate
  , cvv2 = starWords <$> cc.cvv2
  , customer = maskCustomer <$> cc.customer
  }

maskCreditCardNumber :: Text -> Text
maskCreditCardNumber number = firstDigits <> stars <> lastDigits
  where
    (firstDigits, rest) = Text.splitAt 4 number
    toDrop = Text.length rest - 4
    (_, lastDigits) = Text.splitAt toDrop rest
    stars = Text.replicate toDrop "*"

data ExpirationDate = ExpirationDate
  { year :: Word
    -- ^ Last 2 digits of the year in range from 0 (2000) to 99 (2099).
    -- No check is performed whether the value fits into the range.
  , month :: Word
    -- ^ Month of year, in range 1 (January) to 12 (December).
    -- No check is performed whether the value fits into the range.
  }

instance ToJSON ExpirationDate where
  toJSON expiry =
    toJSON (printf "%02d%02d" expiry.year expiry.month :: String)

maskExpirationDate :: LogMasker ExpirationDate
maskExpirationDate _ = ExpirationDate
  { year = 0
  , month = 0
  }

data Customer = Customer
  { address1 :: Text
  , address2 :: Maybe Text
  , zip :: Maybe Text
  }

maskCustomer :: LogMasker Customer
maskCustomer customer = Customer
  { address1 = starWords customer.address1
  , address2 = starWords <$> customer.address2
  , zip = customer.zip
  }

-- | Currently unpopulated
data RiskData = RiskData

instance ToJSON RiskData where
  toJSON RiskData = Object mempty

maskRiskData :: LogMasker RiskData
maskRiskData = id

data AuthResponse = AuthResponse
  { guid :: AuthOnlyGuid
  , status :: TransactionStatus
  , processorStatusCode :: Maybe Text
  , processorResponseMessage :: Maybe Text
  , addressVerificationCode :: Maybe Text
  , cvvVerificationCode :: Maybe Text
  } deriving stock (Show)

-- | Transaction status in Connexpay
-- The list is taken from https://docs.connexpay.com/reference/search-sales
data TransactionStatus
  = TransactionApproved
  | TransactionApprovedWarning
  | TransactionCreatedLocal
    -- ^ Seems to only exist in a test environment, indicates success.
  | TransactionDeclined
  | TransactionCreatedProcNotReached
    -- ^ Communication error between Connexpay and Card Processor
  | TransactionCreatedProcError
    -- ^ Processor errored out
  | TransactionOther Text
    -- ^ In case they return something unexpected
  deriving stock (Show)

instance FromJSON TransactionStatus where
  parseJSON = withText "TransactionStatus" $ pure . \case
    "Transaction - Approved" -> TransactionApproved
    "Transaction - Approved - Warning" -> TransactionApprovedWarning
    "Transaction - CreatedLocal" -> TransactionCreatedLocal
    "Transaction - Declined" -> TransactionDeclined
    "Transaction - Created - Error: Processor not reached" ->
      TransactionCreatedProcNotReached
    "Transaction - Processor Error" -> TransactionCreatedProcError
    other -> TransactionOther other

data AuthError
  = CVVFailed -- ^ CVV verification failure
  | CardInvalid -- ^ Credit card details are invalid
  | InvalidAmount -- ^ Money amount is invalid
  | GeneralDecline -- ^ They just decline
  | OtherAuthError Text -- ^ Some other processing error with 422 code
  deriving stock (Show)

-- | Guess failure type from error string.
guessAuthError :: Error () -> AuthError
guessAuthError err = case err.message of
  "Error code D2020. CVV2 verification failed." -> CVVFailed
  "Error code D2005. Invalid Card." -> CardInvalid
  "Amount field don't allow a value greater than $999,999.99" -> InvalidAmount
  "Error code D2999. General CardAuth Decline." -> GeneralDecline
  txt -> OtherAuthError txt

data VoidRequest
  = VoidAuthorized AuthOnlyGuid
  | VoidCaptured SaleGuid (Maybe USD)

instance ToJSON VoidRequest where
  toJSON = \case
    VoidAuthorized pid -> object ["AuthOnlyGuid" .= pid]
    VoidCaptured pid mbAmount -> object $ catMaybes
      [ Just $ "SaleGuid" .= pid
      , ("Amount" .=) <$> mbAmount
      ]

-- | Internal data type for Capture requests.
data CPTransaction = CPTransaction
  { expectedPayments :: Int32
  }

instance ToJSON CPTransaction where
  toJSON t = object [ "ExpectedPayments" .= t.expectedPayments ]

-- | Response for the payment capture request
data CaptureResponse = CaptureResponse
  { guid :: CaptureGuid
  , sale :: Sale
  } deriving stock (Show)

data Sale = Sale
  { guid :: SaleGuid
  , status :: TransactionStatus
  } deriving stock (Show)

concat <$> sequence
  [ deriveToJSON aesonOptions ''AuthRequest
  , deriveToJSON aesonOptions ''CreditCard
  , deriveToJSON aesonOptions ''Customer
  , deriveFromJSON defaultOptions ''AuthResponse
  , deriveFromJSON defaultOptions ''CaptureResponse
  , deriveFromJSON defaultOptions ''Sale
  ]
