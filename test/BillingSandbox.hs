{-# LANGUAGE OverloadedStrings #-}
-- | Opt-in functional tests. Every request is pinned to Billing.Sandbox.
-- All data is created by the suite. Subscriptions/invoices are canceled and
-- catalog entries are archived on success or failure; records remain inspectable.
module Main (main) where

import BillingSandboxSupport
import Control.Monad (forM_, void, when)
import Data.Aeson (Value(Object), object, (.=))
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Word (Word64)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Numeric (showHex)
import qualified Paddle.Billing.Client as Billing
import Paddle.Billing.Address
import Paddle.Billing.Catalog
import Paddle.Billing.Customer
import Paddle.Billing.Portal
import Paddle.Billing.Settings
import Paddle.Billing.Subscription
import Paddle.Billing.Types
import Paddle.Billing.Transaction
import Servant.Client (ClientM)
import System.Random (randomIO)

main :: IO ()
main = do
  token <- loadSandboxKey
  manager <- newTlsManager
  nonce <- randomIO :: IO Word64
  let tag = "paddle-functional-" <> Text.pack (showHex nonce "")
  withCleanups $ \registerCleanup -> runSuite manager token tag registerCleanup

runSuite :: Manager -> Text -> Text -> (String -> IO () -> IO ()) -> IO ()
runSuite manager token tag registerCleanup = do
  let api = Billing.client (Billing.ApiKey token)
      request :: String -> ClientM (Response a) -> IO a
      request = sandboxRequest manager
      originalEmail = tag <> "@example.com"
      updatedEmail = tag <> "+billing@example.com"
      metadata = case object ["functional_test_run" .= tag] of
        Object value -> value
        _ -> mempty

  Text.putStrLn ("Running against sandbox-api.paddle.com. Test run: " <> tag)
  prepareCheckout manager token tag registerCleanup
  created <- request "create customer" $
    Billing.customersCreate api (newCustomer originalEmail)
      { createCustomerName = Just tag
      , createCustomerCustomData = Just metadata
      }
  let customer = customerId created
  Text.putStrLn ("Created customer: " <> unId customer)
  check "created customer email" (customerEmail created == originalEmail)
  check "created customer name" (customerName created == Just tag)

  updated <- request "update customer email and clear name" $
    Billing.customersUpdate api customer emptyCustomerUpdate
      { updateCustomerEmail = Just updatedEmail
      , updateCustomerName = Clear
      }
  check "updated customer ID" (customerId updated == customer)
  readCustomer <- request "read back customer" (Billing.customersGet api customer)
  check "customer email persisted" (customerEmail readCustomer == updatedEmail)
  check "customer name cleared" (customerName readCustomer == Nothing)
  check "omitted custom data preserved" (customerCustomData readCustomer == Just metadata)
  check "omitted locale preserved" (customerLocale readCustomer == customerLocale created)

  createdAddress <- request "create address" $
    Billing.addressesCreate api customer (newAddress "GB")
      { createAddressFirstLine = Just "123 Main Street"
      , createAddressSecondLine = Just "Suite 2"
      , createAddressCity = Just "London"
      , createAddressPostalCode = Just "SW1A 1AA"
      , createAddressRegion = Just "Greater London"
      , createAddressDescription = Just tag
      }
  let address = addressId createdAddress
  Text.putStrLn ("Created address: " <> unId address)
  check "address belongs to customer" (addressCustomerId createdAddress == customer)
  updatedAddress <- request "update address and clear second line" $
    Billing.addressesUpdate api customer address emptyAddressUpdate
      { updateAddressFirstLine = Set "456 Updated Street"
      , updateAddressCity = Set "Manchester"
      , updateAddressPostalCode = Set "M1 1AE"
      , updateAddressRegion = Set "Greater Manchester"
      , updateAddressSecondLine = Clear
      }
  check "updated address ID" (addressId updatedAddress == address)
  readAddress <- request "read back address" (Billing.addressesGet api customer address)
  check "address line persisted" (addressFirstLine readAddress == Just "456 Updated Street")
  check "address city persisted" (addressCity readAddress == Just "Manchester")
  check "address postcode persisted" (addressPostalCode readAddress == Just "M1 1AE")
  check "address region persisted" (addressRegion readAddress == Just "Greater Manchester")
  check "second line cleared" (addressSecondLine readAddress == Nothing)
  check "omitted country preserved" (addressCountryCode readAddress == "GB")
  check "omitted description preserved" (addressDescription readAddress == Just tag)

  secondAddress <- request "create second address for pagination" $
    Billing.addressesCreate api customer (newAddress "GB")
      { createAddressPostalCode = Just "SW1A 1AA"
      , createAddressFirstLine = Just "789 Alternate Street"
      , createAddressCity = Just "London"
      , createAddressRegion = Just "Greater London"
      , createAddressDescription = Just tag
      }
  Text.putStrLn ("Created address: " <> unId (addressId secondAddress))
  checkAddressPages manager token customer [address, addressId secondAddress]

  subscriptions <- request "list fresh customer's subscriptions" $
    Billing.subscriptionsList api (Just customer) Nothing (Just 1)
  check "fresh customer has no subscriptions" (null subscriptions)
  -- Invoices require a customer name, so restore it after testing null clearing.
  void $ request "restore customer name for invoicing" $
    Billing.customersUpdate api customer emptyCustomerUpdate {updateCustomerName = Set tag}
  createdProduct <- request "create product" $
    Billing.productsCreate api (CreateProduct tag "standard" (Just metadata))
  let productIdentifier = productId createdProduct
  Text.putStrLn ("Created product: " <> unId productIdentifier)
  registerCleanup "archive product" $ do
    archived <- request "archive product" (Billing.productsArchive api productIdentifier Archive)
    check "product archived" (productStatus archived == "archived")
  readProduct <- request "read back product" (Billing.productsGet api productIdentifier)
  check "product name persisted" (productName readProduct == tag)
  check "product metadata persisted" (productCustomData readProduct == Just metadata)

  price <- request "create recurring price" $
    Billing.pricesCreate api (CreatePrice productIdentifier tag (Money "1000" "GBP")
      (Just (BillingCycle "month" 1)))
  let priceIdentifier = priceId price
  Text.putStrLn ("Created price: " <> unId priceIdentifier)
  registerCleanup "archive price" $ do
    archived <- request "archive price" (Billing.pricesArchive api priceIdentifier Archive)
    check "price archived" (priceStatus archived == Just "archived")
  readPrice <- request "read back price" (Billing.pricesGet api priceIdentifier)
  check "price belongs to product" (priceProductId readPrice == unId productIdentifier)
  check "price amount persisted" (priceUnitPrice readPrice == Money "1000" "GBP")
  check "recurring billing cycle persisted" (priceBillingCycle readPrice == Just (BillingCycle "month" 1))

  invoice <- request "create manually-collected invoice" $
    Billing.transactionsCreate api (newInvoice customer address "GBP" [TransactionItem priceIdentifier 1])
      {createTransactionCustomData = Just metadata}
  let invoiceIdentifier = transactionId invoice
  Text.putStrLn ("Created invoice transaction: " <> unId invoiceIdentifier)
  registerCleanup "cancel invoice" $ do
    current <- request "get invoice for cleanup" (Billing.transactionsGet api invoiceIdentifier)
    when (transactionStatus current /= "canceled") $ void $ request "cancel invoice" $
      Billing.transactionsUpdate api invoiceIdentifier cancelTransaction
  check "invoice ready to issue" (transactionStatus invoice == "ready")
  check "manual collection" (transactionCollectionMode invoice == Just "manual")
  check "invoice customer" (transactionCustomerId invoice == Just customer)
  check "invoice address" (transactionAddressId invoice == Just address)
  check "checkout disabled" (fmap billingEnableCheckout (transactionBillingDetails invoice) == Just False)
  void $ request "update draft invoice quantity" $
    Billing.transactionsUpdate api invoiceIdentifier (UpdateTransaction Nothing (Just [TransactionItem priceIdentifier 2]))
  -- Register before issuing: even a lost billing response may have created a subscription.
  registerCleanup "cancel invoice subscription" (cancelInvoiceSubscription manager token invoiceIdentifier)
  issued <- request "issue recurring invoice" $
    Billing.transactionsUpdate api invoiceIdentifier billTransaction
  check "invoice issued" (transactionStatus issued == "billed")
  -- Paddle may attach the subscription ID asynchronously after issuing.
  linked <- pollUntil "invoice subscription link" (
    request "get invoice subscription link" (Billing.transactionsGet api invoiceIdentifier))
    ((/= Nothing) . transactionSubscriptionId)
  identifier <- maybe (fail "Issued invoice has no subscription ID.") pure (transactionSubscriptionId linked)
  Text.putStrLn ("Created subscription: " <> unId identifier)
  before <- pollUntil "active subscription" (
    request "get created subscription" (Billing.subscriptionsGet api identifier))
    ((== "active") . subscriptionStatus)
  check "subscription customer" (subscriptionCustomerId before == customer)
  check "subscription address" (subscriptionAddressId before == address)
  check "subscription collection mode" (subscriptionCollectionMode before == "manual")
  check "subscription items" (itemQuantities before == [(priceIdentifier, 2)])
  let desired = replaceItems DoNotBill [UpdateItem priceIdentifier (Just 3)]
  preview <- request "preview subscription quantity change" $
    Billing.subscriptionsPreview api identifier desired
  check "preview quantities" (sort [(priceId (itemPrice item), itemQuantity item) | item <- previewItems preview] == [(priceIdentifier, 3)])
  afterPreview <- request "verify preview did not apply changes" (Billing.subscriptionsGet api identifier)
  check "preview is not a mutation" (itemQuantities afterPreview == itemQuantities before)
  void $ request "apply quantity and billing address change" $
    Billing.subscriptionsUpdate api identifier desired
      {updateSubscriptionAddressId = Just (addressId secondAddress)}
  changed <- request "read back subscription changes" (Billing.subscriptionsGet api identifier)
  check "quantity change persisted" (itemQuantities changed == [(priceIdentifier, 3)])
  check "address change persisted" (subscriptionAddressId changed == addressId secondAddress)
  listed <- request "list customer's created subscription" $
    Billing.subscriptionsList api (Just customer) Nothing (Just 10)
  check "subscription appears in list" (identifier `elem` map subscriptionId listed)

  portal <- request "create portal session with subscription links" $
    Billing.portalSessionsCreate api customer (CreatePortalSession [identifier])
  check "portal belongs to customer" (portalSessionCustomerId portal == customer)
  check "portal overview link" ("https://" `Text.isPrefixOf` portalOverview (portalUrlsGeneral (portalSessionUrls portal)))
  check "portal subscription links" (map portalLinkId (portalUrlsSubscriptions (portalSessionUrls portal)) == [identifier])
  extraInvoice <- request "create second invoice for pagination" $
    Billing.transactionsCreate api (newInvoice customer address "GBP" [TransactionItem priceIdentifier 1])
  registerCleanup "cancel pagination invoice" $ do
    current <- request "get pagination invoice for cleanup" (Billing.transactionsGet api (transactionId extraInvoice))
    when (transactionStatus current /= "canceled") $ void $ request "cancel pagination invoice" $
      Billing.transactionsUpdate api (transactionId extraInvoice) cancelTransaction
  let page after = do
        result <- Billing.runClient manager Billing.Sandbox $
          Billing.transactionsList api (Just customer) Nothing after (Just 1)
        response <- either (fail . describeError) pure result
        check "transaction page request ID" (not (Text.null (metaRequestId (responseMeta response))))
        check "transaction page customer filter" $
          all ((== Just customer) . transactionCustomerId) (responseData response)
        pagination <- maybe (fail "Missing transaction pagination") pure (metaPagination (responseMeta response))
        check "one transaction per page" (length (responseData response) == 1)
        pure (map transactionId (responseData response), pagination)
  (firstIds, firstPage) <- page Nothing
  check "transaction page has more" (paginationHasMore firstPage)
  cursor <- case firstIds of
    [value] -> pure value
    _ -> fail "Expected one transaction cursor"
  (secondIds, secondPage) <- page (Just cursor)
  check "last transaction page" (not (paginationHasMore secondPage))
  check "pagination contains both invoices without duplicates" $
    sort (firstIds <> secondIds) == sort [invoiceIdentifier, transactionId extraInvoice]
  forM_ [Nothing, Just customer] $ \customerFilter -> do
    invoices <- request "list subscription transactions" $
      Billing.transactionsList api customerFilter (Just identifier) Nothing (Just 30)
    check "subscription filter excludes unrelated invoice" (map transactionId invoices == [invoiceIdentifier])
  forM_ [1 :: Int, 2] $ \_ -> do
    pdf <- request "generate invoice PDF URL on demand" (Billing.transactionsInvoice api invoiceIdentifier)
    check "invoice PDF URL uses HTTPS" ("https://" `Text.isPrefixOf` invoicePdfUrl pdf)
  scheduled <- request "schedule cancellation" $
    Billing.subscriptionsCancel api identifier (CancelSubscription NextBillingPeriod)
  check "cancellation scheduled" (fmap scheduledAction (subscriptionScheduledChange scheduled) == Just "cancel")
  void $ request "remove scheduled cancellation" $
    Billing.subscriptionsUpdate api identifier emptySubscriptionUpdate {updateSubscriptionRemoveScheduledChange = True}
  unscheduled <- request "read back removed cancellation" (Billing.subscriptionsGet api identifier)
  check "subscription remains active" (subscriptionStatus unscheduled == "active")
  check "scheduled cancellation removed" (subscriptionScheduledChange unscheduled == Nothing)
  void $ request "cancel subscription immediately" $
    Billing.subscriptionsCancel api identifier (CancelSubscription Immediately)
  canceled <- pollUntil "canceled subscription" (
    request "read back canceled subscription" (Billing.subscriptionsGet api identifier))
    ((== "canceled") . subscriptionStatus)
  check "canceled subscription ID" (subscriptionId canceled == identifier)
  void $ request "cancel issued invoice" $
    Billing.transactionsUpdate api invoiceIdentifier cancelTransaction
  canceledInvoice <- request "read back canceled invoice" (Billing.transactionsGet api invoiceIdentifier)
  check "invoice cancellation persisted" (transactionStatus canceledInvoice == "canceled")
  putStrLn "Sandbox assertions passed; cleaning up the created catalog entries."

-- Bootstrap only an unset sandbox setting and restore it last during cleanup.
-- Register restoration before writing so a lost response cannot skip cleanup.
prepareCheckout :: Manager -> Text -> Text -> (String -> IO () -> IO ()) -> IO ()
prepareCheckout manager token tag registerCleanup = do
  let api = Billing.client (Billing.ApiKey token)
      request :: String -> ClientM (Response a) -> IO a
      request = sandboxRequest manager
  original <- request "get default payment link" (Billing.checkoutUrlGet api)
  case checkoutDefaultUrl original of
    Just _ -> putStrLn "Using the existing sandbox default payment link."
    Nothing -> do
      let temporary = CheckoutUrlSettings (Just ("https://localhost/" <> tag))
      registerCleanup "restore default payment link" $ do
        current <- request "get default payment link for cleanup" (Billing.checkoutUrlGet api)
        if current == original then pure ()
        else if current == temporary then do
          restored <- request "restore original default payment link" (Billing.checkoutUrlUpdate api original)
          check "default payment link restored" (restored == original)
        else fail "Sandbox default payment link changed externally; leaving it untouched."
      updated <- request "set temporary sandbox default payment link" (Billing.checkoutUrlUpdate api temporary)
      check "temporary default payment link set" (updated == temporary)

cancelInvoiceSubscription :: Manager -> Text -> TransactionId -> IO ()
cancelInvoiceSubscription manager token identifier = do
  let api = Billing.client (Billing.ApiKey token)
      request :: String -> ClientM (Response a) -> IO a
      request = sandboxRequest manager
  current <- request "get invoice for subscription cleanup" (Billing.transactionsGet api identifier)
  linked <- if transactionSubscriptionId current == Nothing && transactionStatus current == "billed"
    then pollUntil "subscription link during cleanup"
      (request "wait for invoice subscription cleanup" (Billing.transactionsGet api identifier))
      ((/= Nothing) . transactionSubscriptionId)
    else pure current
  case transactionSubscriptionId linked of
    Nothing -> pure ()
    Just subscription -> do
      value <- request "get subscription for cleanup" (Billing.subscriptionsGet api subscription)
      when (subscriptionStatus value /= "canceled") $ void $ request "cancel subscription during cleanup" $
        Billing.subscriptionsCancel api subscription (CancelSubscription Immediately)

itemQuantities :: Subscription -> [(PriceId, Int)]
itemQuantities value = sort
  [(priceId (itemPrice item), itemQuantity item) | item <- subscriptionItems value]

checkAddressPages :: Manager -> Text -> CustomerId -> [AddressId] -> IO ()
checkAddressPages manager token customer expected = do
  let api = Billing.client (Billing.ApiKey token)
      page after = do
        result <- Billing.runClient manager Billing.Sandbox $
          Billing.addressesList api customer after (Just 1)
        case result of
          Left err -> fail ("list addresses: " <> describeError err)
          Right response -> do
            check "address page has request ID" (not (Text.null (metaRequestId (responseMeta response))))
            pagination <- maybe (fail "Address list response is missing pagination.") pure
              (metaPagination (responseMeta response))
            check "address page size" (length (responseData response) == 1)
            check "address page customer" (all ((== customer) . addressCustomerId) (responseData response))
            pure (map addressId (responseData response), pagination)
  (firstIds, firstPagination) <- page Nothing
  check "address page one has more" (paginationHasMore firstPagination)
  cursor <- case firstIds of
    [identifier] -> pure identifier
    _ -> fail "Expected exactly one address on the first page."
  (secondIds, secondPagination) <- page (Just cursor)
  check "address page two is last" (not (paginationHasMore secondPagination))
  let identifiers = firstIds <> secondIds
  check "pages contain both created addresses exactly once"
    (length identifiers == 2 && all (`elem` identifiers) expected && firstIds /= secondIds)
  putStrLn "PASS address pagination"
