module Paddle.Billing.Address where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Paddle.Billing.Types

data Address = Address
  { addressId :: AddressId
  , addressCustomerId :: CustomerId
  , addressDescription :: Maybe Text
  , addressFirstLine :: Maybe Text
  , addressSecondLine :: Maybe Text
  , addressCity :: Maybe Text
  , addressPostalCode :: Maybe Text
  , addressRegion :: Maybe Text
  , addressCountryCode :: Text
  , addressStatus :: Text
  , addressCustomData :: Maybe Object
  , addressCreatedAt :: UTCTime
  , addressUpdatedAt :: UTCTime
  , addressImportMeta :: Maybe ImportMeta
  } deriving (Eq, Show, Generic)

instance FromJSON Address where
  parseJSON = genericParseJSON (jsonOptions 7)

data CreateAddress = CreateAddress
  { createAddressCountryCode :: Text
  , createAddressDescription :: Maybe Text
  , createAddressFirstLine :: Maybe Text
  , createAddressSecondLine :: Maybe Text
  , createAddressCity :: Maybe Text
  , createAddressPostalCode :: Maybe Text
  , createAddressRegion :: Maybe Text
  , createAddressCustomData :: Maybe Object
  } deriving (Eq, Show, Generic)

instance ToJSON CreateAddress where
  toJSON = genericToJSON (jsonOptions 13)

-- | A country is required. Some countries also require a postal code.
newAddress :: Text -> CreateAddress
newAddress country = CreateAddress country Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data UpdateAddress = UpdateAddress
  { updateAddressCountryCode :: Maybe Text
  , updateAddressDescription :: Patch Text
  , updateAddressFirstLine :: Patch Text
  , updateAddressSecondLine :: Patch Text
  , updateAddressCity :: Patch Text
  , updateAddressPostalCode :: Patch Text
  , updateAddressRegion :: Patch Text
  , updateAddressCustomData :: Patch Object
  } deriving (Eq, Show)

instance ToJSON UpdateAddress where
  toJSON update = object $
    optionalField "country_code" (updateAddressCountryCode update)
    <> patchField "description" (updateAddressDescription update)
    <> patchField "first_line" (updateAddressFirstLine update)
    <> patchField "second_line" (updateAddressSecondLine update)
    <> patchField "city" (updateAddressCity update)
    <> patchField "postal_code" (updateAddressPostalCode update)
    <> patchField "region" (updateAddressRegion update)
    <> patchField "custom_data" (updateAddressCustomData update)

emptyAddressUpdate :: UpdateAddress
emptyAddressUpdate = UpdateAddress Nothing Unchanged Unchanged Unchanged Unchanged Unchanged Unchanged Unchanged
