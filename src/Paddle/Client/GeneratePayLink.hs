module Paddle.Client.GeneratePayLink where

import Data.Aeson (ToJSON, toJSON, genericToJSON)
import Protolude
import Prelude ()
import Paddle.FieldModifier (customJSONOptions)

data GeneratePayLink = GeneratePayLink 
  { vendorId :: Int
  , vendorAuthCode :: Text
  , productId :: Maybe Integer
  , prices :: Maybe [Text]
  , customMessage :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON GeneratePayLink where
  toJSON = genericToJSON customJSONOptions
