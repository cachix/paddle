module Paddle.Client.DeleteModifier where

import Data.Aeson (ToJSON, toJSON, genericToJSON)
import Protolude
import Prelude ()
import Paddle.FieldModifier (customJSONOptions)


data DeleteModifier = DeleteModifier 
  { vendorId :: Int
  , vendorAuthCode :: Text
  , modifierId :: Integer
  } deriving (Show, Generic)

instance ToJSON DeleteModifier where
  toJSON = genericToJSON customJSONOptions
