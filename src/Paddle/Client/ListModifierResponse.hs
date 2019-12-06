module Paddle.Client.ListModifierResponse where

import Data.Aeson (FromJSON, parseJSON, genericParseJSON)
import Protolude
import Prelude ()
import Paddle.FieldModifier (customJSONOptions)


data ListModifierResponse = ListModifierResponse
  { modifierId :: Integer
  , subscriptionId :: Integer
  , amount :: Double
  } deriving (Show, Generic)

instance FromJSON ListModifierResponse where
  parseJSON = genericParseJSON customJSONOptions