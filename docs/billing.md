# Paddle Billing

The Billing client supports:

- Customers: create, get, update email, name, locale, and custom data.
- Addresses: create, list, get, update.
- Subscriptions: list, get, update, preview changes, cancel.
- Customer portal sessions: generate temporary authenticated management links.
- Catalog: create, get, and archive products and prices.
- Transactions: create, list with filters, get, update items, issue/cancel invoices,
  and fetch invoice PDF URLs.
- Checkout URL settings: get, set, and clear the default payment link.

Use a TLS-enabled HTTP manager, for example one created with `newTlsManager`
from `http-client-tls`. Keep API keys on your server.

```haskell
import Data.Text (Text)
import Network.HTTP.Client (Manager)
import qualified Paddle.Billing.Client as Billing
import Paddle.Billing.Customer
import Paddle.Billing.Types

updateEmail
  :: Manager -> Text -> CustomerId -> Text
  -> IO (Either Billing.BillingClientError (Response Customer))
updateEmail manager token customer email =
  Billing.runClient manager Billing.Sandbox $
    Billing.customersUpdate (Billing.client (Billing.ApiKey token)) customer
      emptyCustomerUpdate { updateCustomerEmail = Just email }
```

Select `Sandbox` or `Live` explicitly. `client` adds bearer authentication and
pins `Paddle-Version: 1`. IDs use `Id "ctm_..."`, `Id "add_..."`, etc.; the
resource types prevent passing an address ID where a customer ID is required.
IDs are not validated locally. Use IDs returned by Paddle.

PATCH fields use `Nothing` or `Unchanged` to leave a field alone. Nullable
fields also accept `Clear` to send JSON `null`, or `Set value` to replace them.
For example, set `updateAddressSecondLine = Clear` to remove a second line.

After changing an address, check the subscription's `subscriptionAddressId`.
Use `updateSubscriptionAddressId = Just address` if it should reference a
different address. These are separate API operations.

Use `replaceItems mode items` to change subscription items with an explicit
proration policy. The item list must include every item you want to retain.
Pass the same update to `subscriptionsPreview` before `subscriptionsUpdate`
to show the customer the proposed charges. Previews have a separate response
type because they are not persisted subscriptions. Pricing breakdowns remain
JSON objects; monetary amounts in them are strings in the smallest currency
denomination. Billing date changes also require a proration policy.

List endpoints accept an `after` ID and `per_page`. Read `responseMeta` for
pagination, check `paginationHasMore`, and pass the last returned entity's ID
as the next cursor while retaining your filters. `paginationNext` also retains
Paddle's complete next-page URL. No requests are retried automatically.

Successes return `Response { responseData, responseMeta }`. API failures return
`BillingApiError` with Paddle's error code, validation details, request ID, and
the original Servant error. Transport and unrecognized HTTP failures return
`BillingTransportError` with the original error, preserving HTTP status and
headers for application retry decisions.

## Invoice tables

`transactionsList api customer subscription after perPage` returns
`Response [Transaction]`. Each argument is optional (`Maybe`); combine customer
and subscription IDs to narrow the table. Retain both filters when passing the
last transaction's ID as `after` on the next page. Check `metaPagination` and
`paginationHasMore`; Paddle allows up to 30 transactions per page.

Store transaction IDs in invoice rows. When a user opens an invoice, call
`transactionsInvoice api transactionId` and use `invoicePdfUrl` from its
`Response InvoicePdf`. Fetch a fresh URL on each click: the signed URL expires
after one hour. Both endpoints require `transaction.read`. PDFs are available
for billed/completed manual transactions and completed automatic transactions,
excluding zero-value transactions. See [Paddle's invoice PDF endpoint](https://developer.paddle.com/api-reference/transactions/get-transaction-invoice/).

## Billing webhooks

Read the original request body as bytes and the `Paddle-Signature` header,
then call `verifyWebhook secret signatureHeader rawBody` from
`Paddle.Billing.WebHook`. It checks HMAC-SHA256 with constant-time digest
comparison and a five-second timestamp tolerance before decoding JSON.
`WebhookSecret` is the notification destination's secret, separate from your
API key and Classic signature key. Keep your server clock synchronized.

`verifyWebhookAt` accepts an explicit tolerance and current time for testing
or application-specific requirements. It rejects timestamps outside the
tolerance in either direction and accepts multiple `h1` signatures for rotation.

The event envelope exposes `eventId`, `eventType`, `eventOccurredAt`, and
`eventData`. Supported payloads include subscription lifecycle events,
transaction events, customer/address changes, and customer, address, and
subscription imports. Unrecognized event types preserve their JSON payload as
`UnknownEvent`. Persist event IDs to deduplicate deliveries and handle events
that arrive out of order using their occurrence times. Verification alone does
not deduplicate events. Acknowledge after durable processing or queuing.

## Signup and migration

New recurring subscriptions can be created through Paddle.js v2 checkout
using price IDs configured in the dashboard. Paddle creates the subscription
automatically; there is no direct create subscription endpoint. This initial
client also supports creating manually-collected transactions and issuing
recurring invoices, which create subscriptions without checkout. Refunds,
pause/resume, and business tax-detail updates are not included.

During migration, keep Classic handling for existing Classic subscriptions and
use Billing for new subscriptions. Store which system owns each subscription.
Handle `customer.imported` and `subscription.imported`, using `import_meta` to
map Classic references to the new Billing IDs. Migrating records is a separate
Paddle dashboard operation, not something this library performs.

References: [Billing API](https://developer.paddle.com/api-reference/),
[signature verification](https://developer.paddle.com/webhooks/about/signature-verification/),
[migration checklist](https://developer.paddle.com/migrate/start/).
