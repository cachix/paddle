# Changelog for paddle

## Unreleased changes

* Add customer/subscription-filtered transaction pagination and on-demand invoice PDF URLs.
* Pin sandbox TLS dependencies compatible with the current crypton backend.
* Add a Haskell sandbox checkout runner with real webhook delivery, signature verification, and optional browser card payment.
* Bootstrap and restore an unset sandbox payment link, preserve credentials through Nix, and include invoice-required address regions.
* Bootstrap sandbox subscriptions through recurring invoices and test changes, cancellation, and cleanup.
* Add Billing product, price, and transaction clients for sandbox provisioning and invoice workflows.
* Add opt-in Paddle sandbox functional tests for billing details and subscription previews.
* Add Paddle Billing clients for customers, addresses, subscriptions, and portal sessions.
* Add typed Billing IDs, explicit PATCH clearing, response pagination, and structured errors.
* Add signed Billing webhook decoding, including migration events, and Billing tests.
* Initial release
