-- https://developer.paddle.com/webhook-reference/subscription-alerts/subscription-cancelled
module Paddle.WebHook.SubscriptionCancelled where

import Protolude
import Prelude ()

data SubscriptionCancelled passthrough = SubscriptionCancelled
  { subscriptionId :: Text
  , subscriptionPlanId :: Text
  , cancellationEffectiveDate :: Text
  , passthrough :: passthrough
  } deriving (Generic, Show)
