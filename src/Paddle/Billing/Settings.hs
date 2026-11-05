module Paddle.Billing.Settings where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Paddle.Billing.Types (jsonOptions)

-- | A null default URL clears the account setting. This applies to the entire
-- account; applications should preserve existing settings when testing.
newtype CheckoutUrlSettings = CheckoutUrlSettings
  { checkoutDefaultUrl :: Maybe Text
  } deriving (Eq, Show, Generic)

instance FromJSON CheckoutUrlSettings where
  parseJSON = genericParseJSON (jsonOptions 8)

instance ToJSON CheckoutUrlSettings where
  toJSON = genericToJSON ((jsonOptions 8) {omitNothingFields = False})
