module Paddle.Client.ListModifierResponse where

import Data.Aeson (FromJSON, parseJSON, genericParseJSON)
import Protolude
import Prelude ()
import Paddle.FieldModifier (customJSONOptions)
import Paddle.Amount

data ListModifierResponse = ListModifierResponse
  { modifierId :: Integer
  , subscriptionId :: Integer
  , amount :: Amount
  } deriving (Show, Generic)

instance FromJSON ListModifierResponse where
  parseJSON = genericParseJSON customJSONOptions