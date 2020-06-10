module Paddle.Client.SubscriptionUsersUpdateResponse where

import Data.Aeson (FromJSON, parseJSON, genericParseJSON)
import Protolude
import Prelude ()
import Paddle.FieldModifier (customJSONOptions)

data SubscriptionUsersUpdateResponse = SubscriptionUsersUpdateResponse
  { subscriptionId :: Integer
  , planId :: Integer
  , userId :: Integer
  -- TODO: nextPayment
  } deriving (Show, Generic)

instance FromJSON SubscriptionUsersUpdateResponse where
  parseJSON = genericParseJSON customJSONOptions
