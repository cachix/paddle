module Paddle.Client.CreateModifierResponse where

import Data.Aeson (FromJSON, parseJSON, genericParseJSON)
import Protolude
import Prelude ()
import Paddle.FieldModifier (customJSONOptions)
  
data CreateModifierResponse = CreateModifierResponse
  { modifierId :: Integer
  , subscriptionId :: Integer
  } deriving (Show, Generic)

instance FromJSON CreateModifierResponse where
  parseJSON = genericParseJSON customJSONOptions