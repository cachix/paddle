# Haskell API for [Paddle](https://paddle.com) payments

Implements parts of Paddle Classic and Paddle Billing. These are separate APIs
with separate customer and subscription records.

- `Paddle.Client`, `Paddle.WebHook`: existing Classic integration.
- `Paddle.Billing.Client`, `Paddle.Billing.WebHook`: Billing integration.

## Paddle Billing

The Billing client supports:

- Customers: create, get, update email, name, locale, and custom data.
- Addresses: create, list, get, update.
- Subscriptions: list, get, update, preview changes, cancel.
- Customer portal sessions: generate temporary authenticated management links.
- Catalog: create, get, and archive products and prices.
- Transactions: create, get, update items, issue invoices, cancel invoices.
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

## Development

Run `stack test` using the resolver in `stack.yaml`. The Billing tests use
synthetic JSON fixtures, independent HMAC test vectors, and a temporary loopback
HTTP server. They do not call Paddle or require API credentials.

### Sandbox functional tests

The separate `billing-sandbox-tests` suite makes real requests to
`sandbox-api.paddle.com`. It is disabled by default, including in CI. Enable it
explicitly and provide a sandbox API key through the environment:

```bash
read -r -s -p 'Sandbox API key: ' PADDLE_SANDBOX_API_KEY
export PADDLE_SANDBOX_API_KEY
stack --no-nix-pure test --flag paddle:sandbox-tests paddle:test:billing-sandbox-tests
```

Create the key in your Paddle sandbox account with these permissions:
`customer.read`, `customer.write`, `address.read`, `address.write`,
`product.read`, `product.write`, `price.read`, `price.write`,
`transaction.read`, `transaction.write`, `subscription.read`,
`subscription.write`, `checkout_settings.read`, `checkout_settings.write`,
and `customer_portal_session.write`. A missing key fails
the enabled suite instead of reporting a successful skipped run. The host is
fixed to the sandbox; the suite has no live-mode option. API keys and generated
portal URLs are not printed.

`--no-nix-pure` preserves the API-key environment variable when Stack uses a Nix
shell. Pure Nix shells otherwise remove it before running the executable.

No existing customers, products, prices, transactions, or subscriptions are
needed. The sandbox account must have Billing enabled and the `standard` product
tax category available. Paddle requires an account-level default payment link
for invoice creation, even when checkout is disabled. The suite preserves an
existing link; if it is unset, it temporarily sets a unique `https://localhost/`
URL and restores the original null value during cleanup. This URL is never opened.
See [default payment link setup](https://developer.paddle.com/build/transactions/default-payment-link/).

Each run creates uniquely named records and exercises the following workflow:

1. Create a customer and two addresses. Update the billing email and address,
   clear nullable fields, check omitted fields are preserved, and paginate addresses.
2. Create and retrieve a product and a GBP 10 monthly price.
3. Create a manually-collected invoice, update its quantity, and issue it.
   Paddle creates the subscription when the recurring invoice is issued, without
   card entry. The suite waits for the subscription ID to become available.
4. Retrieve the subscription, preview a quantity change, confirm that the preview
   did not apply it, then apply quantity and address changes with `do_not_bill`.
5. List the customer's subscription and generate portal links for it.
6. Schedule cancellation, reverse it, and cancel immediately. Cancel the invoice
   and read both records back to check the final state.

The suite uses only records created in the current run. It no longer needs
`PADDLE_SANDBOX_SUBSCRIPTION_ID`. Created IDs are printed as they become available;
API keys and authenticated portal links are not printed. It makes real sandbox
mutations and issues an invoice to a unique `example.com` test address.

Cleanup runs on both success and failure, canceling the created subscription and
invoice before archiving the price and product and restoring any temporary
default payment link. Every registered cleanup action
is attempted; a cleanup failure makes the run fail. Customer/address records and
canceled/archived financial and catalog records remain available for inspection.
If an API response is lost before a created record's ID is known, inspect the
uniquely named run in the dashboard for any additional records.

The suite tests the manually-collected subscription lifecycle. The separate
checkout harness below tests real webhook delivery and card payment. Renewals
and refunds are not covered.

See [API authentication](https://developer.paddle.com/api-reference/about/authentication/)
for creating API keys,
[issuing recurring invoices](https://developer.paddle.com/build/invoices/create-issue-invoices/),
and [subscription previews](https://developer.paddle.com/api-reference/subscriptions/preview-subscription-update/).

### Real webhook delivery and card checkout

Run `test/paddle_checkout_sandbox.py` from any directory with Python 3 and Stack
available. It uses the sandbox API for administrative setup and compiles
`test/BillingWebhookVerify.hs` to verify incoming signatures and decode events
with this library. Build the project with `stack build` first. For a precompiled
verifier, pass `--verifier /absolute/path/to/verify-webhook`.

Start a temporary HTTPS tunnel to `http://127.0.0.1:8765`, for example:

```bash
cloudflared tunnel --url http://127.0.0.1:8765
```

Keep the tunnel running. In a second terminal, export the sandbox API key as
above and use the HTTPS origin printed by the tunnel:

```bash
python3 test/paddle_checkout_sandbox.py --public-url https://YOUR-TUNNEL.trycloudflare.com
```

The harness checks public reachability before creating records. It creates a
temporary notification destination and a customer, rejects missing/tampered
signatures over HTTP, then requires a real, correctly signed `customer.created`
delivery. Event IDs are deduplicated in memory for this test run. Add
`--webhook-only` to stop after this step without creating a card checkout.

For the default card test it also creates a client-side token, product, recurring
GBP 10 price, address, and automatically collected transaction. Open the printed
checkout URL in a browser. Use sandbox card `4242 4242 4242 4242`, any future
expiry, and any three-digit CVC. Browser entry is manual; the harness waits up to
600 seconds (`--checkout-wait` changes this). It requires a completed transaction,
a captured payment, an active subscription, and signed `transaction.completed`
and `subscription.created` deliveries. A timeout fails the test. Webhook delivery
has a separate 120-second timeout (`--webhook-wait`).

In addition to the customer, address, catalog, transaction, subscription, and
checkout settings permissions above, this harness needs
`notification_setting.write` and `client_token.write`. It does not use a live
endpoint. Only the client-side token appears in the checkout page. API keys,
webhook secrets, raw events, and payment details are not logged.

Cleanup is attempted on success, failure, and Ctrl-C: cancel the subscription or
unpaid transaction, archive the price/product, revoke the temporary token,
restore the previous default payment URL, and delete the temporary notification
destination. If the URL changed externally, leave it alone and report cleanup
failure. Stop the tunnel afterward. Completed sandbox payments and customer/
address records remain for inspection. A lost create response or forcibly killed
process may require dashboard cleanup using the printed run tag and IDs.

See Paddle's [sandbox test cards](https://developer.paddle.com/sdks/sandbox/),
[transaction checkout](https://developer.paddle.com/build/transactions/pass-transaction-checkout/),
and [notification destinations](https://developer.paddle.com/api-reference/notification-settings/create-notification-setting/).
