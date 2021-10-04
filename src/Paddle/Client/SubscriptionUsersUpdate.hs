-- https://developer.paddle.com/api-reference/subscription-api/subscription-users/updateuser
module Paddle.Client.SubscriptionUsersUpdate where

import Data.Aeson (ToJSON, toJSON, genericToJSON)
import Protolude
import Prelude ()
import Paddle.FieldModifier (customJSONOptions)


data SubscriptionUsersUpdate = SubscriptionUsersUpdate 
  { vendorId :: Int
  , vendorAuthCode :: Text
  , subscriptionId :: Integer
  , planId :: Maybe Integer
  , prorate :: Maybe Bool
  , billImmediately :: Maybe Bool
  } deriving (Show, Generic)

instance ToJSON SubscriptionUsersUpdate where
    toJSON = genericToJSON customJSONOptions
