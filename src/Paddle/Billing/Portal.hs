module Paddle.Billing.Portal where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Paddle.Billing.Types

newtype CreatePortalSession = CreatePortalSession
  { portalSubscriptionIds :: [SubscriptionId]
  } deriving (Eq, Show, Generic)

instance ToJSON CreatePortalSession where
  toJSON = genericToJSON (jsonOptions 6)

-- | Portal URLs contain temporary authentication tokens. Do not persist them.
data PortalSession = PortalSession
  { portalSessionId :: PortalSessionId
  , portalSessionCustomerId :: CustomerId
  , portalSessionUrls :: PortalUrls
  } deriving (Eq, Show, Generic)

instance FromJSON PortalSession where
  parseJSON = genericParseJSON (jsonOptions 13)

data PortalUrls = PortalUrls
  { portalUrlsGeneral :: PortalGeneralUrls
  , portalUrlsSubscriptions :: [PortalSubscriptionUrls]
  } deriving (Eq, Show, Generic)

instance FromJSON PortalUrls where
  parseJSON = genericParseJSON (jsonOptions 10)

newtype PortalGeneralUrls = PortalGeneralUrls
  { portalOverview :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON PortalGeneralUrls where
  parseJSON = genericParseJSON (jsonOptions 6)

data PortalSubscriptionUrls = PortalSubscriptionUrls
  { portalLinkId :: SubscriptionId
  , portalLinkCancelSubscription :: Text
  , portalLinkUpdateSubscriptionPaymentMethod :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON PortalSubscriptionUrls where
  parseJSON = genericParseJSON (jsonOptions 10)
