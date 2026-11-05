module Paddle.Billing.Subscription where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Paddle.Billing.Types

-- | Monetary amounts are strings in the currency's smallest denomination.
data Money = Money
  { moneyAmount :: Text
  , moneyCurrencyCode :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON Money where
  parseJSON = genericParseJSON (jsonOptions 5)

instance ToJSON Money where
  toJSON = genericToJSON (jsonOptions 5)

data BillingCycle = BillingCycle
  { cycleInterval :: Text
  , cycleFrequency :: Int
  } deriving (Eq, Show, Generic)

instance FromJSON BillingCycle where
  parseJSON = genericParseJSON (jsonOptions 5)

instance ToJSON BillingCycle where
  toJSON = genericToJSON (jsonOptions 5)

data BillingPeriod = BillingPeriod
  { periodStartsAt :: UTCTime
  , periodEndsAt :: UTCTime
  } deriving (Eq, Show, Generic)

instance FromJSON BillingPeriod where
  parseJSON = genericParseJSON (jsonOptions 6)

data Price = Price
  { priceId :: PriceId
  , priceProductId :: Text
  , priceDescription :: Text
  , priceUnitPrice :: Money
  , priceBillingCycle :: Maybe BillingCycle
  , priceStatus :: Maybe Text
  } deriving (Eq, Show, Generic)

instance FromJSON Price where
  parseJSON = genericParseJSON (jsonOptions 5)

data SubscriptionItem = SubscriptionItem
  { itemPrice :: Price
  , itemQuantity :: Int
  , itemStatus :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON SubscriptionItem where
  parseJSON = genericParseJSON (jsonOptions 4)

data ScheduledChange = ScheduledChange
  { scheduledAction :: Text
  , scheduledEffectiveAt :: UTCTime
  , scheduledResumeAt :: Maybe UTCTime
  } deriving (Eq, Show, Generic)

instance FromJSON ScheduledChange where
  parseJSON = genericParseJSON (jsonOptions 9)

data Subscription = Subscription
  { subscriptionId :: SubscriptionId
  , subscriptionCustomerId :: CustomerId
  , subscriptionAddressId :: AddressId
  , subscriptionBusinessId :: Maybe BusinessId
  , subscriptionStatus :: Text
  , subscriptionCurrencyCode :: Text
  , subscriptionCollectionMode :: Text
  , subscriptionItems :: [SubscriptionItem]
  , subscriptionBillingCycle :: Maybe BillingCycle
  , subscriptionCurrentBillingPeriod :: Maybe BillingPeriod
  , subscriptionNextBilledAt :: Maybe UTCTime
  , subscriptionScheduledChange :: Maybe ScheduledChange
  , subscriptionCustomData :: Maybe Object
  , subscriptionImportMeta :: Maybe ImportMeta
  , subscriptionCreatedAt :: UTCTime
  , subscriptionUpdatedAt :: UTCTime
  } deriving (Eq, Show, Generic)

instance FromJSON Subscription where
  parseJSON = genericParseJSON (jsonOptions 12)

data ProrationBillingMode
  = ProratedImmediately
  | ProratedNextBillingPeriod
  | FullImmediately
  | FullNextBillingPeriod
  | DoNotBill
  deriving (Eq, Show, Generic)

instance ToJSON ProrationBillingMode where
  toJSON = genericToJSON defaultOptions
    { constructorTagModifier = camelTo2 '_' }

data EffectiveFrom = Immediately | NextBillingPeriod
  deriving (Eq, Show, Generic)

instance ToJSON EffectiveFrom where
  toJSON = genericToJSON defaultOptions
    { constructorTagModifier = camelTo2 '_' }

data SubscriptionDiscount = SubscriptionDiscount
  { discountId :: DiscountId
  , discountEffectiveFrom :: EffectiveFrom
  } deriving (Eq, Show, Generic)

instance ToJSON SubscriptionDiscount where
  toJSON = genericToJSON (jsonOptions 8)

-- | Send the COMPLETE desired item list. Omitted existing items are removed.
data UpdateItem = UpdateItem
  { updateItemPriceId :: PriceId
  , updateItemQuantity :: Maybe Int
  } deriving (Eq, Show, Generic)

instance ToJSON UpdateItem where
  toJSON = genericToJSON (jsonOptions 10)

data UpdateSubscription = UpdateSubscription
  { updateSubscriptionCustomerId :: Maybe CustomerId
  , updateSubscriptionAddressId :: Maybe AddressId
  , updateSubscriptionBusinessId :: Patch BusinessId
  , updateSubscriptionItems :: Maybe [UpdateItem]
  , updateSubscriptionProrationBillingMode :: Maybe ProrationBillingMode
  , updateSubscriptionNextBilledAt :: Maybe UTCTime
  , updateSubscriptionDiscount :: Patch SubscriptionDiscount
  , updateSubscriptionCustomData :: Patch Object
  , updateSubscriptionRemoveScheduledChange :: Bool
  } deriving (Eq, Show)

instance ToJSON UpdateSubscription where
  toJSON update = object $
    optionalField "customer_id" (updateSubscriptionCustomerId update)
    <> optionalField "address_id" (updateSubscriptionAddressId update)
    <> patchField "business_id" (updateSubscriptionBusinessId update)
    <> optionalField "items" (updateSubscriptionItems update)
    <> optionalField "proration_billing_mode" (updateSubscriptionProrationBillingMode update)
    <> optionalField "next_billed_at" (updateSubscriptionNextBilledAt update)
    <> patchField "discount" (updateSubscriptionDiscount update)
    <> patchField "custom_data" (updateSubscriptionCustomData update)
    <> ["scheduled_change" .= Null | updateSubscriptionRemoveScheduledChange update]

emptySubscriptionUpdate :: UpdateSubscription
emptySubscriptionUpdate = UpdateSubscription
  Nothing Nothing Unchanged Nothing Nothing Nothing Unchanged Unchanged False

-- | Supply an explicit proration policy when changing items or the billing date.
replaceItems :: ProrationBillingMode -> [UpdateItem] -> UpdateSubscription
replaceItems mode items = emptySubscriptionUpdate
  { updateSubscriptionItems = Just items
  , updateSubscriptionProrationBillingMode = Just mode
  }

-- | A preview is not a persisted subscription and has no subscription ID.
-- Pricing breakdowns are retained as JSON objects, including string money amounts.
data SubscriptionPreview = SubscriptionPreview
  { previewCustomerId :: CustomerId
  , previewAddressId :: AddressId
  , previewStatus :: Text
  , previewCurrencyCode :: Text
  , previewItems :: [SubscriptionItem]
  , previewImmediateTransaction :: Maybe Object
  , previewNextTransaction :: Maybe Object
  , previewRecurringTransactionDetails :: Maybe Object
  , previewUpdateSummary :: Maybe Object
  } deriving (Eq, Show, Generic)

instance FromJSON SubscriptionPreview where
  parseJSON = genericParseJSON (jsonOptions 7)

newtype CancelSubscription = CancelSubscription
  { cancelEffectiveFrom :: EffectiveFrom
  } deriving (Eq, Show, Generic)

instance ToJSON CancelSubscription where
  toJSON = genericToJSON (jsonOptions 6)
