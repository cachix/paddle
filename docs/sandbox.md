# Sandbox tests

All sandbox tests create their own records and are disabled by default.
Use a Billing sandbox API key, supplied only through the environment:

```sh
read -r -s -p 'Sandbox API key: ' PADDLE_SANDBOX_API_KEY
export PADDLE_SANDBOX_API_KEY
```

The key needs read/write permissions for customers, addresses, products, prices,
transactions, subscriptions, and checkout settings, plus
`customer_portal_session.write`. Checkout/webhook tests additionally need
`notification_setting.write` and `client_token.write`.

## Invoice lifecycle

```sh
stack test --flag paddle:sandbox-tests paddle:test:billing-sandbox-tests
```

Exercises billing email/address changes, explicit null clearing, pagination,
catalog provisioning, recurring invoices, subscription previews and updates,
portal sessions, and scheduled/immediate cancellation. No existing records or
card are needed. The account must support the `standard` product tax category.

## Webhooks and card checkout

Start an HTTPS tunnel to the harness, for example:

```sh
cloudflared tunnel --url http://127.0.0.1:8765
```

In another terminal, with the API key exported, run from the project root:

```sh
stack build --flag paddle:sandbox-tests paddle:exe:billing-checkout
stack exec billing-checkout -- https://YOUR-TUNNEL.trycloudflare.com
```

The Haskell harness creates a notification destination and requires a real signed
`customer.created` webhook. It then prints a checkout URL. Open it and pay using
card `4242 4242 4242 4242`, any future expiry, and any three-digit CVC. The harness
requires a captured card payment, an active subscription, and verified
`transaction.completed` and `subscription.created` webhooks. Card entry is manual;
checkout times out after ten minutes, and each webhook wait after two minutes.
Append `--webhook-only` to test delivery without creating a checkout.

After building, check the HTTP receiver locally without credentials or a tunnel:

```sh
stack exec billing-checkout -- --self-test
```

## Cleanup

Both runners attempt cleanup on success, failure, and interruption. They cancel
subscriptions/unpaid invoices, archive catalog records, and restore temporary
checkout settings. The checkout runner also revokes its token and removes its
notification destination. A setting changed externally is left untouched and
reported as a cleanup failure. Stop the tunnel afterward.

Customers, addresses, and completed sandbox payments remain for inspection.
Inspect the printed run tag/IDs if a create response is lost or the process is
forcibly killed. Secrets and raw webhook/payment payloads are not logged.
With Stack's pure Nix integration, add `--no-nix-pure` to preserve the key variable.

See [Paddle sandbox documentation](https://developer.paddle.com/sdks/sandbox/).
