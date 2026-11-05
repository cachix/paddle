# Paddle for Haskell

Clients and webhook verification for Paddle Classic and Paddle Billing.

- **Classic:** `Paddle.Client` and `Paddle.WebHook`.
- **Billing:** `Paddle.Billing.Client` and `Paddle.Billing.WebHook`, covering
  customers, addresses, subscriptions, portal sessions, catalog, and transactions.

See the [Billing guide](docs/billing.md) for usage and migration details.
Classic and Billing use separate credentials and records.

## Tests

```sh
stack test
```

Local tests require no Paddle credentials. See [sandbox testing](docs/sandbox.md)
for the opt-in invoice lifecycle, real webhooks, and browser card checkout.
