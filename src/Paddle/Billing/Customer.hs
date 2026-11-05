module Paddle.Billing.Customer where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Paddle.Billing.Types

data Customer = Customer
  { customerId :: CustomerId
  , customerEmail :: Text
  , customerName :: Maybe Text
  , customerStatus :: Text
  , customerLocale :: Text
  , customerMarketingConsent :: Bool
  , customerCustomData :: Maybe Object
  , customerCreatedAt :: UTCTime
  , customerUpdatedAt :: UTCTime
  , customerImportMeta :: Maybe ImportMeta
  } deriving (Eq, Show, Generic)

instance FromJSON Customer where
  parseJSON = genericParseJSON (jsonOptions 8)

data CreateCustomer = CreateCustomer
  { createCustomerEmail :: Text
  , createCustomerName :: Maybe Text
  , createCustomerLocale :: Maybe Text
  , createCustomerCustomData :: Maybe Object
  } deriving (Eq, Show, Generic)

instance ToJSON CreateCustomer where
  toJSON = genericToJSON (jsonOptions 14)

newCustomer :: Text -> CreateCustomer
newCustomer email = CreateCustomer email Nothing Nothing Nothing

data UpdateCustomer = UpdateCustomer
  { updateCustomerEmail :: Maybe Text
  , updateCustomerName :: Patch Text
  , updateCustomerLocale :: Maybe Text
  , updateCustomerCustomData :: Patch Object
  } deriving (Eq, Show)

instance ToJSON UpdateCustomer where
  toJSON update = object $
    optionalField "email" (updateCustomerEmail update)
    <> patchField "name" (updateCustomerName update)
    <> optionalField "locale" (updateCustomerLocale update)
    <> patchField "custom_data" (updateCustomerCustomData update)

emptyCustomerUpdate :: UpdateCustomer
emptyCustomerUpdate = UpdateCustomer Nothing Unchanged Nothing Unchanged
