module Paddle.Client.CreateModifier where

import Data.Aeson (ToJSON, toJSON, genericToJSON)
import Protolude
import Prelude ()
import Paddle.Amount (Amount)
import Paddle.FieldModifier (customJSONOptions)

data CreateModifier = CreateModifier 
  { vendorId :: Int
  , vendorAuthCode :: Text
  , subscriptionId :: Text
  , modifierRecurring :: Bool
  , modifierAmount :: Amount
  , modifierDescription :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON CreateModifier where
  toJSON = genericToJSON customJSONOptions
