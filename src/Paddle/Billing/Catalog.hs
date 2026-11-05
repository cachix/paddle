-- | The catalog operations needed to provision recurring products and prices.
module Paddle.Billing.Catalog where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Paddle.Billing.Subscription (BillingCycle, Money)
import Paddle.Billing.Types

data Product = Product
  { productId :: ProductId
  , productName :: Text
  , productTaxCategory :: Text
  , productStatus :: Text
  , productCustomData :: Maybe Object
  } deriving (Eq, Show, Generic)

instance FromJSON Product where
  parseJSON = genericParseJSON (jsonOptions 7)

data CreateProduct = CreateProduct
  { createProductName :: Text
  , createProductTaxCategory :: Text
  , createProductCustomData :: Maybe Object
  } deriving (Eq, Show, Generic)

instance ToJSON CreateProduct where
  toJSON = genericToJSON (jsonOptions 13)

data CreatePrice = CreatePrice
  { createPriceProductId :: ProductId
  , createPriceDescription :: Text
  , createPriceUnitPrice :: Money
  , createPriceBillingCycle :: Maybe BillingCycle
  } deriving (Eq, Show, Generic)

instance ToJSON CreatePrice where
  toJSON = genericToJSON (jsonOptions 11)

-- | Narrow PATCH payload for archiving test catalog entries or retired plans.
data Archive = Archive deriving (Eq, Show)

instance ToJSON Archive where
  toJSON _ = object ["status" .= ("archived" :: Text)]
