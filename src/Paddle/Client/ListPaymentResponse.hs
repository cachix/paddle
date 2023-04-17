module Paddle.Client.ListPaymentResponse where

import Data.Aeson (FromJSON, parseJSON, genericParseJSON)
import Protolude
import Prelude ()
import Paddle.FieldModifier (customJSONOptions)
import Paddle.Amount

data ListPaymentResponse = ListPaymentResponse
  { id :: Integer
  , subscriptionId :: Integer
  , amount :: Amount
  , currency :: Text
  , payoutDate :: Text
  , isPaid :: Integer
  , receiptUrl :: Text
  , isOneOffCharge :: Bool
  } deriving (Show, Generic)

instance FromJSON ListPaymentResponse where
  parseJSON = genericParseJSON customJSONOptions