module Paddle.Client.GeneratePayLinkResponse where

import Data.Aeson (FromJSON, parseJSON, genericParseJSON)
import Protolude
import Prelude ()
import Paddle.FieldModifier (customJSONOptions)

data GeneratePayLinkResponse = GeneratePayLinkResponse
  { url :: Text
  } deriving (Show, Generic)

instance FromJSON GeneratePayLinkResponse where
  parseJSON = genericParseJSON customJSONOptions