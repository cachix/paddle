-- | Verify the original request bytes BEFORE decoding JSON. The secret belongs
-- to a notification destination, not to the API key or the Classic public key.
module Paddle.Billing.WebHook
  ( WebhookSecret(..)
  , WebhookError(..)
  , Event(..)
  , EventData(..)
  , Transaction(..)
  , verifySignature
  , verifyWebhookAt
  , verifyWebhook
  ) where

import Control.Monad (unless)
import Crypto.Hash (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac)
import Data.Aeson
import Data.ByteArray (constEq, convert)
import Data.ByteArray.Encoding (Base(Base16), convertFromBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isSpace)
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Paddle.Billing.Address
import Paddle.Billing.Customer
import Paddle.Billing.Subscription
import Paddle.Billing.Types
import Paddle.Billing.Transaction (Transaction(..))

newtype WebhookSecret = WebhookSecret ByteString

instance Show WebhookSecret where
  show _ = "WebhookSecret <redacted>"

data WebhookError
  = EmptyWebhookSecret
  | InvalidSignatureHeader
  | InvalidTimestampTolerance
  | TimestampOutsideTolerance
  | SignatureMismatch
  | InvalidEvent String
  deriving (Eq, Show)

data Event = Event
  { eventId :: EventId
  , eventType :: Text
  , eventOccurredAt :: UTCTime
  , eventNotificationId :: Maybe Text
  , eventData :: EventData
  } deriving (Eq, Show)

data EventData
  = SubscriptionEvent Subscription
  | CustomerEvent Customer
  | AddressEvent Address
  | TransactionEvent Transaction
  | UnknownEvent Value
  deriving (Eq, Show)

instance FromJSON Event where
  parseJSON = withObject "Billing event" $ \value -> do
    kind <- value .: "event_type"
    payload <- value .: "data"
    body <- if kind `elem` subscriptionEvents
      then SubscriptionEvent <$> parseJSON payload
      else if kind `elem` ["customer.created", "customer.updated", "customer.imported"]
        then CustomerEvent <$> parseJSON payload
        else if kind `elem` ["address.created", "address.updated", "address.imported"]
          then AddressEvent <$> parseJSON payload
          else if kind `elem` transactionEvents
            then TransactionEvent <$> parseJSON payload
            else pure (UnknownEvent payload)
    Event <$> value .: "event_id" <*> pure kind <*> value .: "occurred_at"
      <*> value .:? "notification_id" <*> pure body
    where
      subscriptionEvents =
        [ "subscription.created", "subscription.activated", "subscription.updated"
        , "subscription.canceled", "subscription.paused", "subscription.resumed"
        , "subscription.past_due", "subscription.trialing", "subscription.imported"
        ]
      transactionEvents =
        [ "transaction.created", "transaction.updated", "transaction.ready"
        , "transaction.billed", "transaction.paid", "transaction.completed"
        , "transaction.canceled", "transaction.past_due", "transaction.payment_failed"
        , "transaction.revised"
        ]

-- | Check HMAC-SHA256 in constant time, accepting any h1 during secret rotation.
-- Reject timestamps both older AND newer than the supplied tolerance.
verifySignature
  :: NominalDiffTime -> UTCTime -> WebhookSecret -> ByteString -> ByteString
  -> Either WebhookError ()
verifySignature tolerance now (WebhookSecret secret) header body = do
  unless (not (BS.null secret)) (Left EmptyWebhookSecret)
  unless (tolerance >= 0) (Left InvalidTimestampTolerance)
  fields <- traverse parseField (BSC.split ';' header)
  timestamp <- case [value | ("ts", value) <- fields] of
    [value] | not (BS.null value) && BSC.all (\c -> c >= '0' && c <= '9') value -> Right value
    _ -> Left InvalidSignatureHeader
  seconds <- case BSC.readInteger timestamp of
    Just (value, rest) | BS.null rest -> Right value
    _ -> Left InvalidSignatureHeader
  let signatures = [value | ("h1", value) <- fields]
  unless (not (null signatures)) (Left InvalidSignatureHeader)
  unless (abs (utcTimeToPOSIXSeconds now - fromInteger seconds) <= tolerance)
    (Left TimestampOutsideTolerance)
  let expected = convert (hmac secret (timestamp <> ":" <> body) :: HMAC SHA256) :: ByteString
      matches signature = case convertFromBase Base16 signature of
        Right digest -> BS.length digest == 32 && constEq expected (digest :: ByteString)
        Left _ -> False
  unless (any matches signatures) (Left SignatureMismatch)
  where
    trim = BSC.dropWhile isSpace . BSC.reverse . BSC.dropWhile isSpace . BSC.reverse
    parseField field = case BSC.break (== '=') (trim field) of
      (key, value) | not (BS.null key) && not (BS.null value) -> Right (key, BS.drop 1 value)
      _ -> Left InvalidSignatureHeader

-- | Verify first, then decode. Persist 'eventId' in the application to make
-- webhook processing idempotent; timestamps alone do not prevent duplicates.
verifyWebhookAt
  :: NominalDiffTime -> UTCTime -> WebhookSecret -> ByteString -> ByteString
  -> Either WebhookError Event
verifyWebhookAt tolerance now secret header body = do
  verifySignature tolerance now secret header body
  either (Left . InvalidEvent) Right (eitherDecodeStrict' body)

-- | Paddle's default timestamp tolerance is five seconds.
verifyWebhook :: WebhookSecret -> ByteString -> ByteString -> IO (Either WebhookError Event)
verifyWebhook secret header body = do
  now <- getCurrentTime
  pure (verifyWebhookAt 5 now secret header body)
