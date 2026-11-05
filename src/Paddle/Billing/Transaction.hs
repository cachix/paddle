module Paddle.Billing.Transaction where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Paddle.Billing.Subscription (BillingCycle(..))
import Paddle.Billing.Types

data BillingDetails = BillingDetails
  { billingEnableCheckout :: Bool
  , billingPaymentTerms :: BillingCycle
  , billingPurchaseOrderNumber :: Maybe Text
  , billingAdditionalInformation :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON BillingDetails where
  toJSON = genericToJSON (jsonOptions 7)

instance FromJSON BillingDetails where
  parseJSON = genericParseJSON (jsonOptions 7)

data TransactionItem = TransactionItem
  { transactionItemPriceId :: PriceId
  , transactionItemQuantity :: Int
  } deriving (Eq, Show, Generic)

instance ToJSON TransactionItem where
  toJSON = genericToJSON (jsonOptions 15)

data CreateTransaction = CreateTransaction
  { createTransactionCustomerId :: CustomerId
  , createTransactionAddressId :: AddressId
  , createTransactionItems :: [TransactionItem]
  , createTransactionCurrencyCode :: Text
  , createTransactionCollectionMode :: Text
  , createTransactionBillingDetails :: Maybe BillingDetails
  , createTransactionCustomData :: Maybe Object
  } deriving (Eq, Show, Generic)

instance ToJSON CreateTransaction where
  toJSON = genericToJSON (jsonOptions 17)

-- | Create a ready invoice, then use 'billTransaction' to issue it. Issuing
-- a recurring invoice creates a manually-collected subscription without a card.
newInvoice :: CustomerId -> AddressId -> Text -> [TransactionItem] -> CreateTransaction
newInvoice customer address currency items = CreateTransaction
  customer address items currency "manual"
  (Just (BillingDetails False (BillingCycle "day" 14) Nothing Nothing)) Nothing

data TransactionStatusChange = BillTransaction | CancelTransaction
  deriving (Eq, Show)

instance ToJSON TransactionStatusChange where
  toJSON BillTransaction = String "billed"
  toJSON CancelTransaction = String "canceled"

data UpdateTransaction = UpdateTransaction
  { updateTransactionStatus :: Maybe TransactionStatusChange
  , updateTransactionItems :: Maybe [TransactionItem]
  } deriving (Eq, Show, Generic)

instance ToJSON UpdateTransaction where
  toJSON = genericToJSON (jsonOptions 17)

billTransaction :: UpdateTransaction
billTransaction = UpdateTransaction (Just BillTransaction) Nothing

-- | Cancel with a status-only payload. Other fields are not allowed when
-- canceling. Subscription-generated invoices require the Paddle dashboard.
cancelTransaction :: UpdateTransaction
cancelTransaction = UpdateTransaction (Just CancelTransaction) Nothing

-- | Monetary totals remain JSON objects containing Paddle's string amounts.
-- Optional fields also accommodate transaction webhook payloads.
data Transaction = Transaction
  { transactionId :: TransactionId
  , transactionStatus :: Text
  , transactionCustomerId :: Maybe CustomerId
  , transactionAddressId :: Maybe AddressId
  , transactionSubscriptionId :: Maybe SubscriptionId
  , transactionCollectionMode :: Maybe Text
  , transactionCurrencyCode :: Text
  , transactionBillingDetails :: Maybe BillingDetails
  , transactionInvoiceNumber :: Maybe Text
  , transactionDetails :: Maybe Object
  , transactionCustomData :: Maybe Object
  , transactionCreatedAt :: UTCTime
  , transactionUpdatedAt :: UTCTime
  } deriving (Eq, Show, Generic)

instance FromJSON Transaction where
  parseJSON = genericParseJSON (jsonOptions 11)

-- | Fetch when opening an invoice. Paddle's signed PDF URL expires after an
-- hour; store the transaction ID rather than persisting this URL.
newtype InvoicePdf = InvoicePdf
  { invoicePdfUrl :: Text
  } deriving (Eq)

instance Show InvoicePdf where
  show _ = "InvoicePdf <temporary URL>"

instance FromJSON InvoicePdf where
  parseJSON = withObject "InvoicePdf" $ \value -> InvoicePdf <$> value .: "url"
