module Paddle.Client.ListPayment where

import Data.Aeson (ToJSON, toJSON, genericToJSON)
import Protolude
import Prelude ()
import Paddle.FieldModifier (customJSONOptions)


data ListPayment = ListPayment 
  { vendorId :: Int
  , vendorAuthCode :: Text
  , subscriptionId :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON ListPayment where
    toJSON = genericToJSON customJSONOptions