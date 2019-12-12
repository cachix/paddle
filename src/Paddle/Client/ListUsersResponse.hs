module Paddle.Client.ListUsersResponse where

import Data.Aeson (FromJSON, parseJSON, genericParseJSON)
import Protolude
import Prelude ()
import Paddle.FieldModifier (customJSONOptions)
import Paddle.Amount

data ListUsersResponse = ListUsersResponse
  { subscriptionId :: Integer
  , planId :: Integer
  , userId :: Integer
  , userEmail :: Text
  , marketingConsent :: Bool
  , lastPayment :: Payment
  , nextPayment :: Payment
  , updateUrl :: Text
  , cancelUrl :: Text
  -- TODO: state, signup_date, paused_at, paused_from
  } deriving (Show, Generic)

instance FromJSON ListUsersResponse where
  parseJSON = genericParseJSON customJSONOptions

data Payment = Payment
  { amount :: Amount
  , currency :: Text
  , date :: Text
  } deriving (Show, Generic)

instance FromJSON Payment where
  parseJSON = genericParseJSON customJSONOptions
  