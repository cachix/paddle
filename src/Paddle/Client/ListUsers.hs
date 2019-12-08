module Paddle.Client.ListUsers where

import Data.Aeson (ToJSON, toJSON, genericToJSON)
import Protolude
import Prelude ()
import Paddle.FieldModifier (customJSONOptions)


data ListUsers = ListUsers 
  { vendorId :: Int
  , vendorAuthCode :: Text
  , subscriptionId :: Maybe Text
  , planId :: Maybe Text
  -- TODO: state, page, resultsPerPage
  } deriving (Show, Generic)

instance ToJSON ListUsers where
    toJSON = genericToJSON customJSONOptions
