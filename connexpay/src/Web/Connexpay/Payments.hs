module Web.Connexpay.Payments
  ( AuthRequest(..)
  , USD(..)
  , CreditCard(..)
  , ExpirationDate(..)
  , Customer(..)
  , RiskData(..)
  , AuthResponse(..)
  , TransactionStatus(..)
  , authorisePayment
  , VoidRequest(..)
  , voidPayment
  , CaptureResponse(..)
  , Sale(..)
  , capturePayment
  , cancelPayment
  , returnPayment
  ) where

import Data.Aeson

import Web.Connexpay.Http
import Web.Connexpay.Payments.Types
import Web.Connexpay.Types


-- | Authorise a credit card payment.
authorisePayment
  :: Connexpay -> Env -> AuthRequest -> IO (Response AuthError AuthResponse)
authorisePayment connexpay env raw = guessResponseErrorType guessAuthError <$>
  doRequest connexpay env "authonlys" RequestBody
    { raw
    , logMasker = maskAuthRequest
    }

-- | Void payment
voidPayment :: Connexpay -> Env -> VoidRequest -> IO (Response () ())
voidPayment connexpay env raw = doRequest_ connexpay env "void" RequestBody
  { raw
  , logMasker = id
  }

-- | Capture payment, previously authorised through 'authorisePayment'.
capturePayment :: Connexpay
               -> Env
               -> AuthOnlyGuid -- ^ Sales GUID, obtained from 'authorisePayment'.
               -> IO (Response () CaptureResponse)
capturePayment connexpay env pid = doRequest connexpay env "Captures" RequestBody
  { raw = object
    [ "AuthOnlyGuid" .= pid
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
              -> IO (Response () ())
cancelPayment connexpay env pid = doRequest_ connexpay env "cancel" RequestBody
  { raw = object [ "SaleGuid" .= pid ]
  , logMasker = id
  }

returnPayment :: Connexpay
              -> Env
              -> SaleGuid -- ^ Sales GUID, obtained from 'capturePayment'.
              -> USD
              -> IO (Response () ())
returnPayment connexpay env pid amt = doRequest_ connexpay env "returns" RequestBody
  { raw = object
    [ "SaleGuid" .= pid
    , "Amount" .= amt
    ]
  , logMasker = id
  }
