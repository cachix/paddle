{-# LANGUAGE OverloadedStrings #-}
-- Used by paddle_checkout_sandbox.py. Verify the original bytes with the actual
-- library, then emit only identifiers, never customer details or signatures.
module Main (main) where

import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text)
import Paddle.Billing.Address (addressId, addressCustomerId)
import Paddle.Billing.Customer (customerId)
import Paddle.Billing.Subscription (subscriptionId, subscriptionCustomerId)
import Paddle.Billing.Types (unId)
import Paddle.Billing.WebHook
import System.Environment (getArgs, getEnv)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  signature <- case args of
    [value] -> pure (BSC.pack value)
    _ -> die "Expected one Paddle-Signature argument."
  secret <- WebhookSecret . BSC.pack <$> getEnv "PADDLE_WEBHOOK_SECRET"
  body <- BS.getContents
  result <- verifyWebhook secret signature body
  case result of
    Left _ -> die "Webhook verification failed."
    Right event -> do
      let (entity, customer) = identifiers (eventData event)
      LBS.putStrLn $ encode $ object
        [ "event_id" .= unId (eventId event), "event_type" .= eventType event
        , "entity_id" .= entity, "customer_id" .= customer ]

identifiers :: EventData -> (Maybe Text, Maybe Text)
identifiers (CustomerEvent x) = (Just (unId (customerId x)), Just (unId (customerId x)))
identifiers (AddressEvent x) = (Just (unId (addressId x)), Just (unId (addressCustomerId x)))
identifiers (SubscriptionEvent x) = (Just (unId (subscriptionId x)), Just (unId (subscriptionCustomerId x)))
identifiers (TransactionEvent x) = (Just (unId (transactionId x)), unId <$> transactionCustomerId x)
identifiers (UnknownEvent _) = (Nothing, Nothing)
