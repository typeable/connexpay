module Web.Connexpay.Data where

import Web.Connexpay.Types

import Data.Money (Money, USD)
import Data.Text (Text)

-- | Create sale request type.
-- This is far from the complete description, but should be enough for our cause.
-- See documentation here: https://docs.connexpay.com/reference/getting-started-with-your-api
data CreateSale = CreateSale { deviceGuid :: DeviceGuid
                             , amount :: Money USD
                             , sequenceCode :: Text
                             , orderCode :: Text
                             , statementDesc :: Maybe Text
                             } deriving (Show)

data CreateSaleResponse = CreateSaleResponse { -- Fill this later as needed
                                             } deriving (Show)

-- | Void a previously create sale
-- Documentation: https://docs.connexpay.com/reference/void
data VoidSale = VoidSale { deviceGuid :: DeviceGuid
                         , saleGuid :: SaleGuid
                         , reason :: Maybe Text
                         , amount :: Maybe (Money USD)
                         , sequenceCode :: Text
                         } deriving (Show)

data VoidSaleResponse = VoidSaleResponse { -- Fill this later as needed
                                         } deriving (Show)
