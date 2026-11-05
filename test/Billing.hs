{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent (forkFinally, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, finally, throwIO, try)
import Control.Monad (forM_, unless)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Socket
import qualified Network.Socket.ByteString as Socket
import Paddle.Billing.Address
import Paddle.Billing.Catalog
import qualified Paddle.Billing.Client as Billing
import Paddle.Billing.Customer
import Paddle.Billing.Portal
import Paddle.Billing.Settings
import Paddle.Billing.Subscription
import Paddle.Billing.Transaction
import Paddle.Billing.Types
import Paddle.Billing.WebHook
import Servant.Client (BaseUrl(..), ClientError, ClientM, Scheme(..), mkClientEnv, runClientM)
import System.Exit (exitFailure)
import System.Timeout (timeout)

main :: IO ()
main = do
  let tests =
        [ ("PATCH omission and explicit null", testUpdates)
        , ("Billing response schemas", testResponses)
        , ("HTTP methods, paths, auth, bodies and statuses", testHttp)
        , ("Catalog and invoice HTTP workflow", testCatalogHttp)
        , ("Invoice table filters, pagination, and PDF URLs", testInvoices)
        , ("Structured and non-JSON HTTP errors", testErrors)
        , ("Webhook signatures and replay protection", testSignatures)
        , ("Signed lifecycle, migration and unknown events", testEvents)
        ]
  forM_ tests $ \(name, test) -> do
    result <- try test
    case result of
      Left err -> putStrLn ("FAIL " <> name <> ": " <> show (err :: SomeException)) >> exitFailure
      Right () -> putStrLn ("PASS " <> name)

assert :: String -> Bool -> IO ()
assert message condition = unless condition (fail message)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual message expected actual = assert
  (message <> ": expected " <> show expected <> ", got " <> show actual) (expected == actual)

fixture :: String -> IO LBS.ByteString
fixture name = LBS.readFile ("test/fixtures/" <> name <> ".json")

decodeFixture :: FromJSON a => String -> IO a
decodeFixture name = fixture name >>= either fail pure . eitherDecode

customer :: CustomerId
customer = Id "ctm_00000000000000000000000000"

address :: AddressId
address = Id "add_00000000000000000000000000"

subscription :: SubscriptionId
subscription = Id "sub_00000000000000000000000000"

price :: PriceId
price = Id "pri_00000000000000000000000000"

catalogProduct :: ProductId
catalogProduct = Id "pro_00000000000000000000000000"

invoiceId :: TransactionId
invoiceId = Id "txn_00000000000000000000000000"

testUpdates :: IO ()
testUpdates = do
  assertEqual "clear default payment link explicitly"
    (object ["default_url" .= Null]) (toJSON (CheckoutUrlSettings Nothing))
  assertEqual "create product"
    (object ["name" .= ("Functional test" :: Text), "tax_category" .= ("standard" :: Text)])
    (toJSON (CreateProduct "Functional test" "standard" Nothing))
  assertEqual "create recurring price with string amount"
    (object ["product_id" .= catalogProduct, "description" .= ("Monthly test" :: Text)
            , "unit_price" .= object ["amount" .= ("1000" :: Text), "currency_code" .= ("GBP" :: Text)]
            , "billing_cycle" .= object ["interval" .= ("month" :: Text), "frequency" .= (1 :: Int)]])
    (toJSON (CreatePrice catalogProduct "Monthly test" (Money "1000" "GBP") (Just (BillingCycle "month" 1))))
  assertEqual "manual invoice without checkout"
    (object ["customer_id" .= customer, "address_id" .= address
            , "items" .= [object ["price_id" .= price, "quantity" .= (1 :: Int)]]
            , "currency_code" .= ("GBP" :: Text), "collection_mode" .= ("manual" :: Text)
            , "billing_details" .= object ["enable_checkout" .= False
              , "payment_terms" .= object ["interval" .= ("day" :: Text), "frequency" .= (14 :: Int)]]])
    (toJSON (newInvoice customer address "GBP" [TransactionItem price 1]))
  assertEqual "issue invoice status only" (object ["status" .= ("billed" :: Text)]) (toJSON billTransaction)
  assertEqual "cancel invoice status only" (object ["status" .= ("canceled" :: Text)]) (toJSON cancelTransaction)
  assertEqual "archive catalog entry" (object ["status" .= ("archived" :: Text)]) (toJSON Archive)
  assertEqual "empty customer update" (object []) (toJSON emptyCustomerUpdate)
  assertEqual "email update leaves other fields alone"
    (object ["email" .= ("new@example.com" :: Text)])
    (toJSON emptyCustomerUpdate {updateCustomerEmail = Just "new@example.com"})
  assertEqual "clear name"
    (object ["name" .= Null]) (toJSON emptyCustomerUpdate {updateCustomerName = Clear})
  assertEqual "create customer omits unset fields"
    (object ["email" .= ("new@example.com" :: Text)]) (toJSON (newCustomer "new@example.com"))
  assertEqual "create address"
    (object ["country_code" .= ("GB" :: Text), "postal_code" .= ("SW1A 1AA" :: Text)])
    (toJSON ((newAddress "GB") {createAddressPostalCode = Just "SW1A 1AA"}))
  assertEqual "clear address line, retain other fields"
    (object ["second_line" .= Null, "city" .= ("London" :: Text)])
    (toJSON emptyAddressUpdate {updateAddressSecondLine = Clear, updateAddressCity = Set "London"})
  assertEqual "empty subscription update" (object []) (toJSON emptySubscriptionUpdate)
  assertEqual "replace complete item list with proration"
    (object ["items" .= [object ["price_id" .= price, "quantity" .= (3 :: Int)]]
            , "proration_billing_mode" .= ("prorated_next_billing_period" :: Text)])
    (toJSON (replaceItems ProratedNextBillingPeriod [UpdateItem price (Just 3)]))
  assertEqual "omit unchanged item quantity"
    (object ["price_id" .= price]) (toJSON (UpdateItem price Nothing))
  assertEqual "clear schedule and discount"
    (object ["scheduled_change" .= Null, "discount" .= Null])
    (toJSON emptySubscriptionUpdate
      {updateSubscriptionRemoveScheduledChange = True, updateSubscriptionDiscount = Clear})
  forM_ (zip [ProratedImmediately, ProratedNextBillingPeriod, FullImmediately, FullNextBillingPeriod, DoNotBill]
       ["prorated_immediately", "prorated_next_billing_period", "full_immediately", "full_next_billing_period", "do_not_bill"]) $
    \(mode, expected) -> assertEqual "proration wire value" (String expected) (toJSON mode)

testResponses :: IO ()
testResponses = do
  productResponse <- decodeFixture "product"
  assertEqual "catalog product ID" catalogProduct (productId (responseData productResponse))
  priceResponse <- decodeFixture "price" :: IO (Response Price)
  assertEqual "catalog price ID" price (priceId (responseData priceResponse))
  archivedPriceResponse <- decodeFixture "price-archived" :: IO (Response Price)
  assertEqual "archived price status" (Just "archived") (priceStatus (responseData archivedPriceResponse))
  invoiceResponse <- decodeFixture "transaction-billed"
  let invoice = responseData invoiceResponse
  assertEqual "invoice subscription link" (Just subscription) (transactionSubscriptionId invoice)
  assertEqual "manual invoice" (Just "manual") (transactionCollectionMode invoice)
  assertEqual "invoice checkout disabled" (Just False) (billingEnableCheckout <$> transactionBillingDetails invoice)
  assertEqual "invoice number" (Just "TEST-001") (transactionInvoiceNumber invoice)
  customerResponse <- decodeFixture "customer"
  assertEqual "customer id" customer (customerId (responseData customerResponse))
  assertEqual "Classic import reference" (Just (ImportMeta (Just "42") "paddle_classic"))
    (customerImportMeta (responseData customerResponse))
  addressResponse <- decodeFixture "address"
  assertEqual "address country" "GB" (addressCountryCode (responseData addressResponse))
  subscriptionResponse <- decodeFixture "subscription"
  item <- case subscriptionItems (responseData subscriptionResponse) of
    [value] -> pure value
    _ -> fail "Expected one subscription item in the fixture"
  assertEqual "price id" price (priceId (itemPrice item))
  assertEqual "amount remains a string" "1500" (moneyAmount (priceUnitPrice (itemPrice item)))
  preview <- decodeFixture "preview"
  assertEqual "preview has no persisted id" customer (previewCustomerId (responseData preview))
  assert "preview retains charges" (previewImmediateTransaction (responseData preview) /= Nothing)
  portal <- decodeFixture "portal"
  assertEqual "portal customer" customer (portalSessionCustomerId (responseData portal))
  portalLink <- case portalUrlsSubscriptions (portalSessionUrls (responseData portal)) of
    [value] -> pure value
    _ -> fail "Expected one subscription portal link in the fixture"
  assertEqual "portal deep link" subscription
    (portalLinkId portalLink)
  addresses <- decodeFixture "addresses" :: IO (Response [Address])
  assertEqual "pagination has more" (Just True)
    (paginationHasMore <$> metaPagination (responseMeta addresses))
  let malformed = eitherDecode "{\"data\":{},\"meta\":{\"request_id\":\"x\"}}"
        :: Either String (Response Customer)
  assert "reject malformed customer response" (isLeft malformed)
  assertEqual "sandbox host" "sandbox-api.paddle.com" (baseUrlHost (Billing.baseUrl Billing.Sandbox))
  assertEqual "live uses TLS" Https (baseUrlScheme (Billing.baseUrl Billing.Live))
  assertEqual "API key redacted" "ApiKey <redacted>" (show (Billing.ApiKey "do-not-log"))

-- A real loopback HTTP server checks the Servant-generated requests without
-- contacting Paddle or requiring credentials.
data Captured = Captured ByteString [(ByteString, ByteString)] ByteString

withServer :: Int -> LBS.ByteString -> ClientM a -> IO (Either ClientError a, Captured)
withServer status body action = bracket (socket AF_INET Stream defaultProtocol) close $ \listener -> do
  bind listener (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  listen listener 1
  socketAddress <- getSocketName listener
  port <- case socketAddress of
    SockAddrInet value _ -> pure (fromIntegral value)
    _ -> fail "Expected an IPv4 socket"
  captured <- newEmptyMVar
  worker <- forkFinally
    (bracket (fst <$> accept listener) close $ \connection -> do
      request <- readRequest connection
      Socket.sendAll connection $ BSC.pack ("HTTP/1.1 " <> show status <> " Test\r\n")
        <> "Content-Type: application/json\r\nConnection: close\r\nContent-Length: "
        <> BSC.pack (show (LBS.length body)) <> "\r\n\r\n" <> LBS.toStrict body
      pure request)
    (putMVar captured)
  let run = do
        manager <- newManager defaultManagerSettings
        result <- runClientM action (mkClientEnv manager (BaseUrl Http "127.0.0.1" port ""))
        request <- takeMVar captured >>= either throwIO pure
        pure (result, request)
  completed <- timeout 5000000 (run `finally` killThread worker)
  maybe (fail "Mock HTTP server timed out") pure completed

readRequest :: Socket -> IO Captured
readRequest connection = readHeaders BS.empty
  where
    receive = do
      chunk <- Socket.recv connection 4096
      if BS.null chunk then fail "Incomplete HTTP request" else pure chunk
    readHeaders bytes = case BSC.breakSubstring "\r\n\r\n" bytes of
      (_, rest) | BS.null rest -> receive >>= readHeaders . (bytes <>)
      (headers, rest) -> do
        (requestLine, headerLines) <- case BSC.lines headers of
          line : fields -> pure (BSC.takeWhile (/= '\r') line, fields)
          [] -> fail "Missing HTTP request line"
        let parseHeader line = let (key, value) = BSC.break (== ':') line
              in (BSC.map toLower key, BSC.dropWhile (== ' ') (BSC.takeWhile (/= '\r') (BS.drop 1 value)))
            fields = map parseHeader headerLines
            bodySize = maybe 0 (read . BSC.unpack) (lookup "content-length" fields)
        payload <- readBody bodySize (BS.drop 4 rest)
        pure (Captured requestLine fields payload)
    readBody size bytes
      | BS.length bytes >= size = pure (BS.take size bytes)
      | otherwise = receive >>= readBody size . (bytes <>)

checkRequest :: String -> Int -> ByteString -> Maybe Value -> ClientM a -> IO ()
checkRequest responseFixture status expectedLine expectedBody action = do
  body <- fixture responseFixture
  (result, Captured line headers payload) <- withServer status body action
  case result of
    Left err -> fail (show err)
    Right _ -> pure ()
  assertEqual "request line" expectedLine (normalizeLine line)
  assertEqual "bearer authentication" (Just "Bearer test-api-key") (lookup "authorization" headers)
  assertEqual "API version" (Just "1") (lookup "paddle-version" headers)
  case expectedBody of
    Nothing -> assertEqual "no request body" BS.empty payload
    Just expected -> do
      assert "JSON content type" (maybe False (BS.isPrefixOf "application/json") (lookup "content-type" headers))
      assertEqual "request body" (Right expected) (eitherDecodeStrict' payload)
  where
    -- Some servant-client versions retain an empty query string.
    normalizeLine line = case BSC.words line of
      [method, target, version] -> BSC.unwords
        [method, if "?" `BS.isSuffixOf` target then BS.init target else target, version]
      _ -> line

testHttp :: IO ()
testHttp = do
  let api = Billing.client (Billing.ApiKey "test-api-key")
      customerPath = "/customers/ctm_00000000000000000000000000"
      addressPath = customerPath <> "/addresses/add_00000000000000000000000000"
      subscriptionPath = "/subscriptions/sub_00000000000000000000000000"
      call method path = method <> " " <> path <> " HTTP/1.1"
      email = emptyCustomerUpdate {updateCustomerEmail = Just "new@example.com"}
      addressUpdate = emptyAddressUpdate {updateAddressFirstLine = Set "123 Main Street"}
      update = replaceItems ProratedImmediately [UpdateItem price (Just 2)]
  checkRequest "customer" 201 (call "POST" "/customers") (Just (toJSON (newCustomer "new@example.com")))
    (Billing.customersCreate api (newCustomer "new@example.com"))
  checkRequest "customer" 200 (call "GET" customerPath) Nothing (Billing.customersGet api customer)
  checkRequest "customer" 200 (call "PATCH" customerPath) (Just (toJSON email))
    (Billing.customersUpdate api customer email)
  checkRequest "address" 201 (call "POST" (customerPath <> "/addresses")) (Just (toJSON (newAddress "GB")))
    (Billing.addressesCreate api customer (newAddress "GB"))
  checkRequest "addresses" 200
    (call "GET" (customerPath <> "/addresses?after=add_00000000000000000000000000&per_page=1")) Nothing
    (Billing.addressesList api customer (Just address) (Just 1))
  checkRequest "address" 200 (call "GET" addressPath) Nothing (Billing.addressesGet api customer address)
  checkRequest "address" 200 (call "PATCH" addressPath) (Just (toJSON addressUpdate))
    (Billing.addressesUpdate api customer address addressUpdate)
  checkRequest "subscription" 200 (call "GET" subscriptionPath) Nothing (Billing.subscriptionsGet api subscription)
  checkRequest "subscriptions" 200
    (call "GET" "/subscriptions?customer_id=ctm_00000000000000000000000000&after=sub_00000000000000000000000000&per_page=1") Nothing
    (Billing.subscriptionsList api (Just customer) (Just subscription) (Just 1))
  checkRequest "subscription" 200 (call "PATCH" subscriptionPath) (Just (toJSON update))
    (Billing.subscriptionsUpdate api subscription update)
  checkRequest "preview" 200 (call "PATCH" (subscriptionPath <> "/preview")) (Just (toJSON update))
    (Billing.subscriptionsPreview api subscription update)
  checkRequest "subscription" 200 (call "POST" (subscriptionPath <> "/cancel"))
    (Just (object ["effective_from" .= ("next_billing_period" :: Text)]))
    (Billing.subscriptionsCancel api subscription (CancelSubscription NextBillingPeriod))
  checkRequest "portal" 201 (call "POST" (customerPath <> "/portal-sessions"))
    (Just (object ["subscription_ids" .= [subscription]]))
    (Billing.portalSessionsCreate api customer (CreatePortalSession [subscription]))

testCatalogHttp :: IO ()
testCatalogHttp = do
  let api = Billing.client (Billing.ApiKey "test-api-key")
      productPath = "/products/pro_00000000000000000000000000"
      pricePath = "/prices/pri_00000000000000000000000000"
      transactionPath = "/transactions/txn_00000000000000000000000000"
      call method path = method <> " " <> path <> " HTTP/1.1"
      productRequest = CreateProduct "Functional test" "standard" Nothing
      priceRequest = CreatePrice catalogProduct "Monthly test" (Money "1000" "GBP") (Just (BillingCycle "month" 1))
      invoiceRequest = newInvoice customer address "GBP" [TransactionItem price 1]
  checkRequest "checkout-settings" 200 (call "GET" "/settings/checkout-url") Nothing
    (Billing.checkoutUrlGet api)
  checkRequest "checkout-settings" 200 (call "PATCH" "/settings/checkout-url")
    (Just (object ["default_url" .= Null])) (Billing.checkoutUrlUpdate api (CheckoutUrlSettings Nothing))
  checkRequest "product" 201 (call "POST" "/products") (Just (toJSON productRequest))
    (Billing.productsCreate api productRequest)
  checkRequest "product" 200 (call "GET" productPath) Nothing (Billing.productsGet api catalogProduct)
  checkRequest "product-archived" 200 (call "PATCH" productPath)
    (Just (object ["status" .= ("archived" :: Text)])) (Billing.productsArchive api catalogProduct Archive)
  checkRequest "price" 201 (call "POST" "/prices") (Just (toJSON priceRequest))
    (Billing.pricesCreate api priceRequest)
  checkRequest "price" 200 (call "GET" pricePath) Nothing (Billing.pricesGet api price)
  checkRequest "price-archived" 200 (call "PATCH" pricePath)
    (Just (object ["status" .= ("archived" :: Text)])) (Billing.pricesArchive api price Archive)
  checkRequest "transaction-ready" 201 (call "POST" "/transactions") (Just (toJSON invoiceRequest))
    (Billing.transactionsCreate api invoiceRequest)
  checkRequest "transaction-ready" 200 (call "GET" transactionPath) Nothing (Billing.transactionsGet api invoiceId)
  checkRequest "transaction-ready" 200 (call "PATCH" transactionPath)
    (Just (object ["items" .= [object ["price_id" .= price, "quantity" .= (2 :: Int)]]]))
    (Billing.transactionsUpdate api invoiceId (UpdateTransaction Nothing (Just [TransactionItem price 2])))
  checkRequest "transaction-billed" 200 (call "PATCH" transactionPath)
    (Just (object ["status" .= ("billed" :: Text)])) (Billing.transactionsUpdate api invoiceId billTransaction)
  checkRequest "transaction-canceled" 200 (call "PATCH" transactionPath)
    (Just (object ["status" .= ("canceled" :: Text)])) (Billing.transactionsUpdate api invoiceId cancelTransaction)

testInvoices :: IO ()
testInvoices = do
  let api = Billing.client (Billing.ApiKey "test-api-key")
      call path = "GET " <> path <> " HTTP/1.1"
      customerQuery = "customer_id=ctm_00000000000000000000000000"
      subscriptionQuery = "subscription_id=sub_00000000000000000000000000"
      invoicePath = "/transactions/txn_00000000000000000000000000/invoice"
      list = Billing.transactionsList api
  checkRequest "transactions" 200 (call "/transactions") Nothing
    (list Nothing Nothing Nothing Nothing)
  checkRequest "transactions" 200 (call ("/transactions?" <> customerQuery)) Nothing
    (list (Just customer) Nothing Nothing Nothing)
  checkRequest "transactions" 200 (call ("/transactions?" <> subscriptionQuery)) Nothing
    (list Nothing (Just subscription) Nothing Nothing)
  checkRequest "transactions" 200
    (call ("/transactions?" <> customerQuery <> "&" <> subscriptionQuery <> "&per_page=1")) Nothing
    (list (Just customer) (Just subscription) Nothing (Just 1))
  first <- decodeFixture "transactions" :: IO (Response [Transaction])
  assertEqual "first transaction page" [invoiceId] (map transactionId (responseData first))
  pagination <- maybe (fail "Missing transaction pagination") pure (metaPagination (responseMeta first))
  assert "transaction page has more" (paginationHasMore pagination)
  assert "pagination URL retains customer filter" ("customer_id=" `Text.isInfixOf` paginationNext pagination)
  assert "pagination URL retains subscription filter" ("subscription_id=" `Text.isInfixOf` paginationNext pagination)
  checkRequest "transactions-next" 200
    (call ("/transactions?" <> customerQuery <> "&" <> subscriptionQuery <>
      "&after=txn_00000000000000000000000000&per_page=1")) Nothing
    (list (Just customer) (Just subscription) (Just invoiceId) (Just 1))
  second <- decodeFixture "transactions-next" :: IO (Response [Transaction])
  assertEqual "second transaction page" [Id "txn_00000000000000000000000001"]
    (map transactionId (responseData second))
  assertEqual "last transaction page" (Just False) (paginationHasMore <$> metaPagination (responseMeta second))
  checkRequest "transactions-empty" 200 (call ("/transactions?" <> customerQuery)) Nothing
    (list (Just customer) Nothing Nothing Nothing)
  empty <- decodeFixture "transactions-empty" :: IO (Response [Transaction])
  assertEqual "empty transaction page" [] (responseData empty)
  checkRequest "invoice-pdf" 200 (call invoicePath) Nothing (Billing.transactionsInvoice api invoiceId)
  pdf <- decodeFixture "invoice-pdf" :: IO (Response InvoicePdf)
  assertEqual "PDF URL decoded" "https://example.com/invoice.pdf?signature=test"
    (invoicePdfUrl (responseData pdf))
  assertEqual "temporary PDF URL redacted" "InvoicePdf <temporary URL>" (show (responseData pdf))

testErrors :: IO ()
testErrors = do
  let action = Billing.customersGet (Billing.client (Billing.ApiKey "test-api-key")) customer
  body <- fixture "error"
  forM_ [400, 429] $ \status -> do
    (result, _) <- withServer status body action
    case result of
      Left err -> case Billing.classifyClientError err of
        Billing.BillingApiError parsed _ -> do
          assertEqual "error code" "invalid_field" (apiErrorCode (errorResponseError parsed))
          assertEqual "request id" "test-request" (metaRequestId (errorResponseMeta parsed))
          assertEqual "validation detail" (Just [ValidationError "email" "Must be an email address."])
            (apiErrorErrors (errorResponseError parsed))
        other -> fail (show other)
      Right _ -> fail "Expected HTTP failure"
  (nonJson, _) <- withServer 502 "upstream unavailable" action
  case nonJson of
    Left err -> case Billing.classifyClientError err of
      Billing.BillingTransportError _ -> pure ()
      other -> fail (show other)
    Right _ -> fail "Expected non-JSON HTTP failure"

-- Independent test vector generated with Python's standard hmac/hashlib.
signedHeader :: ByteString
signedHeader = "ts=1700000000;h1=ea7c612de4e0411f4ade52a9fd6d7a29a78dbc97db54a6c63ec594394163168d"

testSignatures :: IO ()
testSignatures = do
  let secret = WebhookSecret "test-secret"
      now = posixSecondsToUTCTime 1700000000
      body = "{\"event_id\":\"test\"}"
      verify = verifySignature 5 now secret
  assertEqual "valid HMAC" (Right ()) (verify signedHeader body)
  assertEqual "tampered bytes" (Left SignatureMismatch) (verify signedHeader (body <> " "))
  assertEqual "wrong secret" (Left SignatureMismatch)
    (verifySignature 5 now (WebhookSecret "wrong-secret") signedHeader body)
  assertEqual "expired signature" (Left TimestampOutsideTolerance)
    (verifySignature 5 (posixSecondsToUTCTime 1700000006) secret signedHeader body)
  assertEqual "future timestamp" (Left TimestampOutsideTolerance)
    (verifySignature 5 (posixSecondsToUTCTime 1699999994) secret signedHeader body)
  assertEqual "tolerance boundary accepted" (Right ())
    (verifySignature 5 (posixSecondsToUTCTime 1700000005) secret signedHeader body)
  assertEqual "negative tolerance" (Left InvalidTimestampTolerance)
    (verifySignature (-1) now secret signedHeader body)
  assertEqual "empty secret" (Left EmptyWebhookSecret)
    (verifySignature 5 now (WebhookSecret "") signedHeader body)
  assertEqual "key rotation" (Right ())
    (verify ("h1=" <> BSC.replicate 64 '0' <> "; " <> signedHeader) body)
  forM_ ["", "h1=abc", "ts=1700000000", "ts=x;h1=abc", "ts=-1;h1=abc",
         "ts=1700000000;ts=1700000000;h1=abc"] $ \header ->
    assertEqual "malformed header" (Left InvalidSignatureHeader) (verify header body)
  forM_ ["ts=1700000000;h1=zz", "ts=1700000000;h1=00"] $ \header ->
    assertEqual "invalid digest" (Left SignatureMismatch) (verify header body)
  assertEqual "secret redacted" "WebhookSecret <redacted>" (show secret)
  case verifyWebhookAt 5 now secret signedHeader body of
    Left (InvalidEvent _) -> pure ()
    other -> fail ("Expected verified but malformed event, got " <> show other)
  assertEqual "verify before JSON decoding" (Left SignatureMismatch)
    (verifyWebhookAt 5 now secret signedHeader "not JSON")

testEvents :: IO ()
testEvents = do
  signatures <- decodeFixture "signatures" :: IO (KM.KeyMap Text)
  forM_ ["subscription-event", "customer-imported", "subscription-imported", "transaction-event", "address-event", "unknown-event"] $ \name -> do
    bytes <- LBS.toStrict <$> fixture name
    header <- maybe (fail "Missing signature fixture") (pure . BSC.pack . showText) (KM.lookup (fromStringKey name) signatures)
    event <- either (fail . show) pure $
      verifyWebhookAt 5 (posixSecondsToUTCTime 1700000000) (WebhookSecret "test-secret") header bytes
    assertEqual "deduplication event id" (Id "evt_00000000000000000000000000") (eventId event)
    case (name, eventData event) of
      ("subscription-event", SubscriptionEvent _) -> pure ()
      ("customer-imported", CustomerEvent _) -> pure ()
      ("subscription-imported", SubscriptionEvent _) -> pure ()
      ("transaction-event", TransactionEvent _) -> pure ()
      ("address-event", AddressEvent _) -> pure ()
      ("unknown-event", UnknownEvent payload) -> assertEqual "unknown payload preserved" (object ["new_field" .= True]) payload
      _ -> fail "Incorrect event dispatch"
  where
    showText = Text.unpack
    fromStringKey = Key.fromText . Text.pack
