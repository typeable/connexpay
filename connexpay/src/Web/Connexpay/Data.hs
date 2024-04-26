{-# LANGUAGE OverloadedStrings #-}
module Web.Connexpay.Data ( TransactionStatus (..)
                          ) where

import Data.Aeson
import Data.Aeson.Types
import Data.Maybe (fromMaybe)
import Data.Text (Text)

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
