{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import BillingSandboxSupport
import Control.Concurrent (forkFinally, killThread, newEmptyMVar, takeMVar, tryPutMVar, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)
import Control.Exception (bracket, catch, throwIO, toException)
import Control.Monad (forM_, unless, void, when)
import Crypto.Hash (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.ByteArray.Encoding (Base(Base16), convertToBase)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types
import Network.Wai (Application, Request, getRequestBodyChunk, pathInfo, requestHeaders, requestMethod, responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setHost, setPort, setTimeout)
import Numeric (showHex)
import qualified Paddle.Billing.Client as Billing
import Paddle.Billing.Address
import Paddle.Billing.Catalog
import Paddle.Billing.Customer
import Paddle.Billing.Settings
import Paddle.Billing.Subscription
import Paddle.Billing.Transaction
import Paddle.Billing.Types
import Paddle.Billing.WebHook
import Servant.Client (ClientM)
import System.Environment (getArgs)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)
import System.Random (randomIO)
import System.Timeout (timeout)

data Receiver = Receiver
  { secret :: IORef (Maybe WebhookSecret)
  , events :: MVar (Map.Map EventId (Text, Text))
  , page :: IORef LBS.ByteString
  }

newReceiver :: IO Receiver
newReceiver = Receiver <$> newIORef Nothing <*> newMVar Map.empty <*> newIORef "Preparing checkout."

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  receiver <- newReceiver
  manager <- newTlsManager
  withReceiver receiver $ case args of
    ["--self-test"] -> receiverTests manager receiver
    [url] -> run manager receiver url False
    [url, "--webhook-only"] -> run manager receiver url True
    _ -> fail "Usage: billing-checkout HTTPS_ORIGIN [--webhook-only] | --self-test"

-- Bind only the dedicated harness to loopback. Startup failures reach the caller.
withReceiver :: Receiver -> IO a -> IO a
withReceiver receiver action = do
  ready <- newEmptyMVar
  let settings = setHost "127.0.0.1" $ setPort 8765 $ setTimeout 5 $
        setBeforeMainLoop (void (tryPutMVar ready (Right ()))) defaultSettings
      finished result = void $ tryPutMVar ready $ case result of
        Left err -> Left err
        Right () -> Left (toException (userError "Receiver stopped"))
  bracket (forkFinally (runSettings settings (application receiver)) finished) killThread $ \_ -> do
    takeMVar ready >>= either throwIO pure
    action

application :: Receiver -> Application
application receiver request respond = do
  let reply status body = respond $ responseLBS status
        [(hContentType, "text/plain"), (hCacheControl, "no-store")] body
  case (requestMethod request, pathInfo request) of
    ("GET", ["health"]) -> reply status200 "paddle-sandbox-harness"
    ("GET", ["checkout"]) -> do
      html <- readIORef (page receiver)
      respond $ responseLBS status200
        [(hContentType, "text/html; charset=utf-8"), (hCacheControl, "no-store")] html
    ("POST", ["webhook"]) -> do
      key <- readIORef (secret receiver)
      raw <- readBoundedBody request
      let signature = maybe "" id (lookup "Paddle-Signature" (requestHeaders request))
      case (key, raw) of
        (Nothing, _) -> reply status503 "Not ready"
        (_, Nothing) -> reply status413 "Body too large"
        (Just webhookSecret, Just body) | BS.length signature <= 4096 -> do
          verified <- verifyWebhook webhookSecret signature body
          case verified of
            Left _ -> reply status400 "Invalid webhook"
            Right event -> do
              case entityId (eventData event) of
                Nothing -> pure ()
                Just entity -> do
                  fresh <- modifyMVar (events receiver) $ \seen -> pure
                    (Map.insert (eventId event) (eventType event, entity) seen,
                     Map.notMember (eventId event) seen)
                  when fresh $ Text.putStrLn ("VERIFIED " <> eventType event <> " " <> entity)
              reply status200 "OK"
        _ -> reply status400 "Invalid signature"
    _ -> reply status404 "Not found"

readBoundedBody :: Request -> IO (Maybe BS.ByteString)
readBoundedBody request = go 0 []
  where
    go size chunks = do
      chunk <- getRequestBodyChunk request
      let total = size + BS.length chunk
      if total > 1048576 then pure Nothing
      else if BS.null chunk then pure (Just (BS.concat (reverse chunks)))
      else go total (chunk : chunks)

entityId :: EventData -> Maybe Text
entityId (CustomerEvent x) = Just (unId (customerId x))
entityId (SubscriptionEvent x) = Just (unId (subscriptionId x))
entityId (TransactionEvent x) = Just (unId (transactionId x))
entityId (AddressEvent x) = Just (unId (addressId x))
entityId (UnknownEvent _) = Nothing

received :: Receiver -> Text -> Text -> IO Bool
received receiver kind entity = elem (kind, entity) . Map.elems <$> readMVar (events receiver)

waitFor :: Int -> String -> IO Bool -> IO ()
waitFor seconds label ready = do
  result <- timeout (seconds * 1000000) loop
  check ("Timed out waiting for " <> label) (result == Just ())
  putStrLn ("PASS " <> label)
  where
    loop = do
      done <- ready
      unless done (threadDelay 2000000 >> loop)

-- Only destination/token administration and payment details use raw JSON. All
-- supported customer/catalog/subscription operations exercise the typed client.
admin :: HTTP.Manager -> Text -> Method -> Text -> Maybe Value -> IO Value
admin manager token method path body = do
  initial <- HTTP.parseRequest (Text.unpack ("https://sandbox-api.paddle.com" <> path))
  let request = HTTP.applyBearerAuth (Text.encodeUtf8 token) initial
        { HTTP.method = method, HTTP.requestBody = HTTP.RequestBodyLBS (maybe "" encode body)
        , HTTP.requestHeaders = [(hContentType, "application/json"), ("Paddle-Version", "1")]
        , HTTP.responseTimeout = HTTP.responseTimeoutMicro 30000000
        , HTTP.checkResponse = \_ _ -> pure () }
  response <- HTTP.httpLbs request manager `catch` \(_ :: HTTP.HttpException) ->
    fail "Sandbox administration transport failed"
  let status = statusCode (HTTP.responseStatus response)
  check ("Sandbox administration HTTP " <> show status) (status >= 200 && status < 300)
  if status == 204 then pure Null else do
    decoded <- either (const (fail "Invalid administration response")) pure
      (eitherDecode (HTTP.responseBody response))
    field "data" decoded

field :: FromJSON a => Key -> Value -> IO a
field key value = either (const (fail ("Invalid response field: " <> show key))) pure $
  parseEither (withObject "response" (.: key)) value

run :: HTTP.Manager -> Receiver -> String -> Bool -> IO ()
run manager receiver url webhookOnly = do
  origin <- HTTP.parseRequest url
  check "Use an HTTPS origin without a path, query, or credentials"
    (HTTP.secure origin && HTTP.port origin == 443 && HTTP.path origin == "/"
      && BS.null (HTTP.queryString origin) && null (HTTP.requestHeaders origin))
  let public = Text.dropWhileEnd (== '/') (Text.pack url)
  health <- HTTP.parseRequest (Text.unpack (public <> "/health")) >>= (`HTTP.httpLbs` manager)
  check "Public URL reaches this receiver" (HTTP.responseBody health == "paddle-sandbox-harness")
  token <- loadSandboxKey
  nonce <- randomIO :: IO Word64
  let tag = "paddle-checkout-" <> Text.pack (showHex nonce "")
      api = Billing.client (Billing.ApiKey token)
      request :: String -> ClientM (Response a) -> IO a
      request = sandboxRequest manager
      administration = admin manager token
  Text.putStrLn ("Run: " <> tag)
  withCleanups $ \cleanup -> do
    destination <- administration "POST" "/notification-settings" $ Just $ object
      [ "description" .= tag, "type" .= ("url" :: Text), "destination" .= (public <> "/webhook")
      , "api_version" .= (1 :: Int), "include_sensitive_fields" .= False
      , "traffic_source" .= ("platform" :: Text)
      , "subscribed_events" .= (["customer.created", "transaction.completed", "subscription.created"] :: [Text]) ]
    destinationId <- field "id" destination
    Text.putStrLn ("Notification destination: " <> destinationId)
    cleanup "delete notification destination" $ void $
      administration "DELETE" ("/notification-settings/" <> destinationId) Nothing
    endpointSecret <- field "endpoint_secret_key" destination
    writeIORef (secret receiver) (Just (WebhookSecret (Text.encodeUtf8 endpointSecret)))
    customer <- request "create customer" $ Billing.customersCreate api
      (newCustomer (tag <> "@example.com")) {createCustomerName = Just tag}
    let customerIdentifier = customerId customer
    waitFor 120 "signed customer.created" (received receiver "customer.created" (unId customerIdentifier))
    unless webhookOnly $ do
      original <- request "get checkout URL" (Billing.checkoutUrlGet api)
      let checkoutUrl = public <> "/checkout"
          temporary = CheckoutUrlSettings (Just checkoutUrl)
      cleanup "restore checkout URL" $ do
        current <- request "read checkout URL" (Billing.checkoutUrlGet api)
        unless (current == original) $ do
          check "Checkout URL changed externally; left untouched" (current == temporary)
          void $ request "restore checkout URL" (Billing.checkoutUrlUpdate api original)
      void $ request "set checkout URL" (Billing.checkoutUrlUpdate api temporary)
      clientToken <- administration "POST" "/client-tokens" (Just (object ["name" .= tag]))
      tokenId <- field "id" clientToken
      cleanup "revoke client token" $ void $ administration "PATCH" ("/client-tokens/" <> tokenId)
        (Just (object ["status" .= ("revoked" :: Text)]))
      createdProduct <- request "create product" (Billing.productsCreate api (CreateProduct tag "standard" Nothing))
      cleanup "archive product" $ void $ request "archive product" (Billing.productsArchive api (productId createdProduct) Archive)
      price <- request "create price" $ Billing.pricesCreate api
        (CreatePrice (productId createdProduct) tag (Money "1000" "GBP") (Just (BillingCycle "month" 1)))
      cleanup "archive price" $ void $ request "archive price" (Billing.pricesArchive api (priceId price) Archive)
      address <- request "create address" $ Billing.addressesCreate api customerIdentifier
        (newAddress "GB") {createAddressFirstLine = Just "123 Main Street", createAddressCity = Just "London",
          createAddressRegion = Just "Greater London", createAddressPostalCode = Just "SW1A 1AA"}
      transaction <- request "create checkout transaction" $ Billing.transactionsCreate api
        (newInvoice customerIdentifier (addressId address) "GBP" [TransactionItem (priceId price) 1])
          {createTransactionCollectionMode = "automatic", createTransactionBillingDetails = Nothing}
      let identifier = transactionId transaction
      Text.putStrLn ("Transaction: " <> unId identifier)
      cleanup "cancel subscription or unpaid transaction" $ do
        current <- request "read checkout for cleanup" (Billing.transactionsGet api identifier)
        forM_ (transactionSubscriptionId current) $ \subscription -> do
          value <- request "read subscription for cleanup" (Billing.subscriptionsGet api subscription)
          when (subscriptionStatus value /= "canceled") $ void $ request "cancel subscription" $
            Billing.subscriptionsCancel api subscription (CancelSubscription Immediately)
        if transactionStatus current `elem` ["draft", "ready"] then void $
          request "cancel unpaid transaction" (Billing.transactionsUpdate api identifier cancelTransaction)
        else check "Transaction requires inspection" (transactionStatus current == "completed")
      publicToken <- field "token" clientToken
      template <- Text.readFile "test/checkout.html"
      let jsonText = Text.decodeUtf8 . LBS.toStrict . encode
      writeIORef (page receiver) $ LBS.fromStrict $ Text.encodeUtf8 $
        Text.replace "TRANSACTION" (jsonText (unId identifier)) (Text.replace "TOKEN" (jsonText (publicToken :: Text)) template)
      Text.putStrLn ("CHECKOUT URL: " <> checkoutUrl)
      waitFor 600 "completed card transaction" $ do
        current <- request "read checkout" (Billing.transactionsGet api identifier)
        pure (transactionStatus current == "completed" && transactionSubscriptionId current /= Nothing)
      paid <- administration "GET" ("/transactions/" <> unId identifier) Nothing
      payments <- field "payments" paid :: IO [Value]
      captured <- mapM (\payment -> do
        status <- field "status" payment
        details <- field "method_details" payment
        kind <- field "type" details
        pure (status == ("captured" :: Text) && kind == ("card" :: Text))) payments
      check "Captured card payment" (or captured)
      linked <- request "read subscription link" (Billing.transactionsGet api identifier)
      subscription <- maybe (fail "Missing subscription") pure (transactionSubscriptionId linked)
      waitFor 60 "active card subscription" $
        (== "active") . subscriptionStatus <$> request "read subscription" (Billing.subscriptionsGet api subscription)
      waitFor 120 "signed transaction.completed" (received receiver "transaction.completed" (unId identifier))
      waitFor 120 "signed subscription.created" (received receiver "subscription.created" (unId subscription))

-- Local HTTP checks exercise the same receiver without Paddle credentials.
receiverTests :: HTTP.Manager -> Receiver -> IO ()
receiverTests manager receiver = do
  let key = "receiver-test-secret"
  writeIORef (secret receiver) (Just (WebhookSecret key))
  raw <- LBS.readFile "test/fixtures/customer-imported.json"
  now <- floor <$> getPOSIXTime :: IO Integer
  let signature timestamp = let ts = BSC.pack (show timestamp) in
        ts <> ";h1=" <> convertToBase Base16 (hmac key (ts <> ":" <> LBS.toStrict raw) :: HMAC SHA256)
      send body header = do
        initial <- HTTP.parseRequest "http://127.0.0.1:8765/webhook"
        response <- HTTP.httpLbs initial {HTTP.method = "POST", HTTP.requestBody = HTTP.RequestBodyLBS body,
          HTTP.requestHeaders = [("Paddle-Signature", header)], HTTP.checkResponse = \_ _ -> pure ()} manager
        pure (statusCode (HTTP.responseStatus response))
      valid = "ts=" <> signature now
  forM_ [(raw, valid, 200), (raw, valid, 200), (raw <> " ", valid, 400),
         (raw, "", 400), (raw, "ts=" <> signature (now - 60), 400),
         (raw, "ts=" <> signature (now + 60), 400)] $ \(body, header, expected) -> do
    status <- send body header
    check "Receiver HTTP status" (status == expected)
  count <- Map.size <$> readMVar (events receiver)
  check "Duplicate events recorded once" (count == 1)
  large <- send (LBS.replicate 1048577 32) valid
  check "Oversized body rejected" (large == 413)
  putStrLn "PASS receiver HTTP signatures, timestamps, deduplication, and body limit"
