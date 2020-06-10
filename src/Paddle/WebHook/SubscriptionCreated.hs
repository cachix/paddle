module Paddle.WebHook.SubscriptionCreated where

import Protolude
import Prelude ()

{-
data SubscriptionStatus =
    Active | Trialing | PastDue | Deleted
    deriving (Show, Eq)

instance FromHttpApiData SubscriptionStatus where
  where
    parseUrlPiece = 

instance FromForm SubscriptionStatus where
  fromForm form = 
    Left (trace ("form" :: Text) $ show form) -- TODO
-}

data SubscriptionCreated passthrough = SubscriptionCreated
  { subscriptionId :: Text
  , subscriptionPlanId :: Text
  , updateUrl :: Text
  , cancelUrl :: Text
  --, status :: SubscriptionStatus
  , passthrough :: passthrough
  } deriving (Generic, Show)
