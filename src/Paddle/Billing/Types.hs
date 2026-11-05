{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
-- | Types shared by the Paddle Billing API. Classic IDs are not interchangeable
-- with these IDs.
module Paddle.Billing.Types where

import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Text (Text)
import GHC.Generics (Generic)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)

type role Id nominal
newtype Id entity = Id { unId :: Text }
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

data CustomerTag
data AddressTag
data SubscriptionTag
data PriceTag
data ProductTag
data DiscountTag
data BusinessTag
data EventTag
data TransactionTag
data PortalSessionTag

type CustomerId = Id CustomerTag
type AddressId = Id AddressTag
type SubscriptionId = Id SubscriptionTag
type PriceId = Id PriceTag
type ProductId = Id ProductTag
type DiscountId = Id DiscountTag
type BusinessId = Id BusinessTag
type EventId = Id EventTag
type TransactionId = Id TransactionTag
type PortalSessionId = Id PortalSessionTag

-- | PATCH fields have three states. Only use 'Clear' for nullable API fields.
-- Non-nullable optional fields use 'Maybe' instead.
data Patch a = Unchanged | Clear | Set a
  deriving (Eq, Show)

patchField :: ToJSON a => Key -> Patch a -> [Pair]
patchField _ Unchanged = []
patchField key Clear = [key .= Null]
patchField key (Set value) = [key .= value]

optionalField :: ToJSON a => Key -> Maybe a -> [Pair]
optionalField _ Nothing = []
optionalField key (Just value) = [key .= value]

-- | Strip a record-specific prefix and convert camelCase to snake_case.
jsonOptions :: Int -> Options
jsonOptions prefixLength = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop prefixLength
  , omitNothingFields = True
  }

data Response a = Response
  { responseData :: a
  , responseMeta :: Meta
  } deriving (Eq, Show, Generic)

instance FromJSON a => FromJSON (Response a) where
  parseJSON = genericParseJSON (jsonOptions 8)

data Meta = Meta
  { metaRequestId :: Text
  , metaPagination :: Maybe Pagination
  } deriving (Eq, Show, Generic)

instance FromJSON Meta where
  parseJSON = genericParseJSON (jsonOptions 4)

data Pagination = Pagination
  { paginationPerPage :: Int
  , paginationNext :: Text
  , paginationHasMore :: Bool
  , paginationEstimatedTotal :: Maybe Int
  } deriving (Eq, Show, Generic)

instance FromJSON Pagination where
  parseJSON = genericParseJSON (jsonOptions 10)

data ImportMeta = ImportMeta
  { importExternalId :: Maybe Text
  , importImportedFrom :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON ImportMeta where
  parseJSON = genericParseJSON (jsonOptions 6)

data ErrorResponse = ErrorResponse
  { errorResponseError :: ApiError
  , errorResponseMeta :: Meta
  } deriving (Eq, Show, Generic)

instance FromJSON ErrorResponse where
  parseJSON = genericParseJSON (jsonOptions 13)

data ApiError = ApiError
  { apiErrorType :: Text
  , apiErrorCode :: Text
  , apiErrorDetail :: Text
  , apiErrorDocumentationUrl :: Maybe Text
  , apiErrorErrors :: Maybe [ValidationError]
  } deriving (Eq, Show, Generic)

instance FromJSON ApiError where
  parseJSON = genericParseJSON (jsonOptions 8)

data ValidationError = ValidationError
  { validationField :: Text
  , validationMessage :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON ValidationError where
  parseJSON = genericParseJSON (jsonOptions 10)
