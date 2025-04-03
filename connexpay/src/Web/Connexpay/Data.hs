{-# LANGUAGE OverloadedStrings #-}
module Web.Connexpay.Data ( TransactionStatus(..)
                          , PaymentFailure(..)
                          , ConnectionError(..)
                          , ConnexpayError(..)
                          , describeFailure
                          , guessFailure
                          , ErrorMessage(..)
                          ) where

import Control.Exception
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.HTTP.Client

-- | Transaction status in Connexpay
-- The list is taken from https://docs.connexpay.com/reference/search-sales
data TransactionStatus = TransactionApproved               -- ^ Obvious
                       | TransactionDeclined               -- ^ Also obvious
                       | TransactionCreatedLocal           -- ^ Seems to only exist in a test environment, indicates success.
                       | TransactionCreatedProcNotReached  -- ^ Communication error between Connexpay and Card Processor
                       | TransactionCreatedProcError       -- ^ Processor errored out
                       | TransactionApprovedWarning        -- ^ Wut 0__o FIXME: figure out what this is
                       | TransactionOther Text             -- ^ In case they return something unexpected
                       deriving (Eq, Ord, Show)

statuses :: [(Text, TransactionStatus)]
statuses = [ ( "Transaction - Approved", TransactionApproved )
           , ( "Transaction - Declined", TransactionDeclined )
           , ( "Transaction - CreatedLocal", TransactionCreatedLocal )
           , ( "Transaction - Created - Error: Processor not reached", TransactionCreatedProcNotReached )
           , ( "Transaction - Processor Error", TransactionCreatedProcError )
           , ( "Transaction - Approved - Warning", TransactionApprovedWarning )
           ]

instance FromJSON TransactionStatus where
  parseJSON (String s) = pure (fromMaybe (TransactionOther s) (lookup s statuses))
  parseJSON v = typeMismatch "TransactionStatus" v

-- | Payment failure types.
--   This type describes failures that related to either credit card being invalid,
--   client account having insufficient funds, and other non-technical conditions.
--   FIXME: this list is not exhaustive. Add more values whenever we encounter them.
data PaymentFailure = CVVFailed         -- ^ CVV verification failure
                    | CardInvalid       -- ^ Credit card details are invalid
                    | InvalidAmount     -- ^ Money amount is invalid
                    | GeneralDecline    -- ^ They just decline
                    | LocalTransaction  -- ^ Special case for transactions that were registered but did't go through somehow.
                    | OtherProcessingError Text -- ^ Some other processing error with 422 code
                    deriving (Eq, Show)

describeFailure :: PaymentFailure -> Text
describeFailure CVVFailed = "CVV authorisation failure"
describeFailure CardInvalid = "Invalid credit card details"
describeFailure InvalidAmount = "Invalid amount of money requested"
describeFailure GeneralDecline = "General card decine"
describeFailure LocalTransaction = "Transaction registered but not processed. Consult with payment processor."
describeFailure (OtherProcessingError txt) = "Transaction declined due to other error: " <> txt

-- | Guess failure type from HTTP code and supplied error string.
guessFailure :: Int -> Text -> Maybe PaymentFailure
guessFailure 422 "Error code D2020. CVV2 verification failed." = Just CVVFailed
guessFailure 422 "Error code D2005. Invalid Card." = Just CardInvalid
guessFailure 422 "Amount field don't allow a value greater than $999,999.99" = Just InvalidAmount
guessFailure 422 "Error code D2999. General CardAuth Decline." = Just GeneralDecline
guessFailure 422 txt = Just (OtherProcessingError txt)
guessFailure _ _ = Nothing

-- | Error response from Connexpay
data ErrorMessage = ErrorMessage { message :: Text
                                 , errorId :: Text }

instance FromJSON ErrorMessage where
  parseJSON (Object o) = ErrorMessage <$> o .: "message"
                                      <*> o .: "errorId"
  parseJSON v = typeMismatch "ErrorMessage" v

data ConnectionError = ParseError String
                     | InvalidUrl String String
                     | HttpFailure HttpExceptionContent
                     | TokenError String
                     deriving Show

-- | Error type for Connexpay.
--   There are two possible cases here:
--   * Connection failure means the payment may or may not have gotten through.
--     This would be typically thrown as an exception in an application.
--   * Payment failure means Connexpay returned an error and the payment wasn't authorised.
--     No exception here, this must be handled as usual.
data ConnexpayError = ConnectionError ConnectionError
                    | PaymentFailure PaymentFailure (Maybe Text)
                    deriving (Show)

instance Exception ConnexpayError
