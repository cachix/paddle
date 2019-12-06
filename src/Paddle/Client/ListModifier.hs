module Paddle.Client.ListModifier where

import Data.Aeson (ToJSON, toJSON, genericToJSON)
import Protolude
import Prelude ()
import Paddle.FieldModifier (customJSONOptions)


data ListModifier = ListModifier 
  { vendorId :: Int
  , vendorAuthCode :: Text
  , subscriptionId :: Maybe Text
  , planId :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON ListModifier where
    toJSON = genericToJSON customJSONOptions
