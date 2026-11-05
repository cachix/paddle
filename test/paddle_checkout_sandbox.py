#!/usr/bin/env python3
"""Real Billing webhooks and optional browser checkout, using only the sandbox.

Administrative setup uses the REST API; signature verification and event decoding
run through the Haskell library. No Python packages beyond the standard library.
The tunnel must forward its HTTPS URL to this harness's loopback port.
"""

import argparse
import json
import os
from pathlib import Path
import secrets
import shutil
import subprocess
import tempfile
import threading
import time
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.error import HTTPError, URLError
from urllib.parse import urlsplit
from urllib.request import Request, urlopen


class Sandbox:
    def __init__(self, key):
        if not key.startswith("pdl_sdbx_apikey_"):
            raise RuntimeError("PADDLE_SANDBOX_API_KEY must be a sandbox API key")
        self.key = key
        self.cleanups = []

    def api(self, method, path, payload=None):
        body = None if payload is None else json.dumps(payload).encode()
        request = Request("https://sandbox-api.paddle.com" + path, body, {
            "Authorization": "Bearer " + self.key,
            "Paddle-Version": "1", "Content-Type": "application/json",
        }, method=method)
        try:
            with urlopen(request, timeout=30) as response:
                raw = response.read()
                return json.loads(raw)["data"] if raw else None
        except HTTPError as error:
            # Never dump request headers, endpoint secrets, or entire responses.
            try:
                result = json.loads(error.read())
                code = result.get("error", {}).get("code", "unknown")
                request_id = result.get("meta", {}).get("request_id", "unknown")
            except (ValueError, KeyError):
                code, request_id = "unknown", "unknown"
            raise RuntimeError(f"{method} {path}: HTTP {error.code}, {code}, request {request_id}") from None
        except URLError:
            raise RuntimeError(f"{method} {path}: connection or TLS failure") from None

    def cleanup(self):
        failed = False
        for label, action in reversed(self.cleanups):
            try:
                action()
                print("PASS cleanup:", label, flush=True)
            except Exception:
                failed = True
                print("FAIL cleanup:", label, "(inspect the printed record IDs)", flush=True)
        return not failed


class Receiver:
    def __init__(self, verifier):
        self.verifier = verifier
        self.secret = None
        self.events = {}
        self.lock = threading.Lock()
        self.page = b"Checkout is being prepared. Reload shortly."

    def received(self, kind, entity):
        with self.lock:
            return any(e["event_type"] == kind and e["entity_id"] == entity
                       for e in self.events.values())

    def handler(self):
        state = self

        class Handler(BaseHTTPRequestHandler):
            def log_message(self, *_args):
                pass

            def reply(self, status, body, content_type="text/plain"):
                self.send_response(status)
                self.send_header("Content-Type", content_type)
                self.send_header("Cache-Control", "no-store")
                self.send_header("Content-Length", str(len(body)))
                self.end_headers()
                self.wfile.write(body)

            def do_GET(self):
                if urlsplit(self.path).path == "/checkout":
                    self.reply(200, state.page, "text/html; charset=utf-8")
                elif self.path == "/health":
                    self.reply(200, b"paddle-sandbox-harness")
                else:
                    self.reply(404, b"Not found")

            def do_POST(self):
                if self.path != "/webhook":
                    return self.reply(404, b"Not found")
                if state.secret is None:
                    return self.reply(503, b"Not ready")
                signature = self.headers.get("Paddle-Signature", "")
                try:
                    size = int(self.headers.get("Content-Length", "0"))
                    if size <= 0 or size > 1024 * 1024 or len(signature) > 4096:
                        return self.reply(400, b"Invalid request")
                    raw = self.rfile.read(size)
                    if len(raw) != size:
                        return self.reply(400, b"Incomplete request")
                    env = {**os.environ, "PADDLE_WEBHOOK_SECRET": state.secret}
                    env.pop("PADDLE_SANDBOX_API_KEY", None)
                    checked = subprocess.run([state.verifier, signature], input=raw,
                                             capture_output=True, timeout=3, env=env)
                    if checked.returncode:
                        return self.reply(400, b"Invalid webhook")
                    event = json.loads(checked.stdout)
                    with state.lock:
                        fresh = event["event_id"] not in state.events
                        state.events[event["event_id"]] = event
                    if fresh:
                        print("VERIFIED", event["event_type"], event["entity_id"], flush=True)
                    self.reply(200, b"OK")
                except (ValueError, subprocess.TimeoutExpired):
                    self.reply(400, b"Invalid webhook")

            def setup(self):
                super().setup()
                self.connection.settimeout(5)

        return Handler


def wait_for(label, predicate, seconds):
    deadline = time.monotonic() + seconds
    next_update = time.monotonic() + 30
    while time.monotonic() < deadline:
        if predicate():
            print("PASS", label, flush=True)
            return
        if time.monotonic() >= next_update:
            print("Waiting for", label, flush=True)
            next_update = time.monotonic() + 30
        time.sleep(2)
    raise RuntimeError("Timed out waiting for " + label)


def checkout_page(token, transaction):
    # Both are issued by Paddle. The API key and webhook secret stay server-side.
    return ("""<!doctype html><html lang="en"><meta charset="utf-8">
<title>Paddle sandbox checkout test</title>
<h1>Paddle sandbox checkout test</h1>
<p>Use test card 4242 4242 4242 4242, any future expiry and any three-digit CVC.</p>
<button onclick="openCheckout()">Open test checkout</button><p id="status"></p>
<script src="https://cdn.paddle.com/paddle/v2/paddle.js"></script>
<script>
Paddle.Environment.set('sandbox');
Paddle.Initialize({token:TOKEN, eventCallback:function(event){
  document.getElementById('status').textContent = event.name;
}});
function openCheckout(){Paddle.Checkout.open({transactionId:TRANSACTION});}
openCheckout();
</script></html>""".replace("TOKEN", json.dumps(token))
            .replace("TRANSACTION", json.dumps(transaction))).encode()


def run(sandbox, receiver, args):
    tag = "paddle-checkout-" + secrets.token_hex(8)
    print("Run:", tag, flush=True)
    destination = sandbox.api("POST", "/notification-settings", {
        "description": tag, "type": "url", "destination": args.public_url + "/webhook",
        "api_version": 1, "include_sensitive_fields": False, "traffic_source": "platform",
        "subscribed_events": ["customer.created", "transaction.completed", "subscription.created"],
    })
    destination_id = destination["id"]
    print("Notification destination:", destination_id, flush=True)
    sandbox.cleanups.append(("delete temporary notification destination", lambda:
                             sandbox.api("DELETE", "/notification-settings/" + destination_id)))
    receiver.secret = destination["endpoint_secret_key"]
    # Reject missing signatures and correctly-shaped signatures for changed bytes.
    import hashlib
    import hmac
    raw = b'{"test":"original"}'
    timestamp = str(int(time.time()))
    digest = hmac.new(receiver.secret.encode(), timestamp.encode() + b":" + raw,
                      hashlib.sha256).hexdigest()
    for signature in ("", "ts=" + timestamp + ";h1=" + digest):
        request = Request(f"http://127.0.0.1:{args.port}/webhook", b'{"test":"tampered"}',
                          {"Paddle-Signature": signature}, method="POST")
        try:
            urlopen(request, timeout=5).close()
            raise RuntimeError("Receiver accepted an invalid signature")
        except HTTPError as error:
            if error.code != 400:
                raise RuntimeError("Unexpected invalid-signature response") from None
    print("PASS HTTP receiver rejects missing and tampered signatures", flush=True)

    customer = sandbox.api("POST", "/customers", {
        "email": tag + "@example.com", "name": tag, "custom_data": {"functional_test_run": tag}})
    customer_id = customer["id"]
    print("Customer:", customer_id, flush=True)
    wait_for("real signed customer.created delivery",
             lambda: receiver.received("customer.created", customer_id), args.webhook_wait)
    if args.webhook_only:
        print("Webhook test passed. Card checkout was explicitly skipped.", flush=True)
        return

    original_url = sandbox.api("GET", "/settings/checkout-url")["default_url"]
    test_url = args.public_url + "/checkout"

    def restore_url():
        current = sandbox.api("GET", "/settings/checkout-url")["default_url"]
        if current == original_url:
            return
        if current != test_url:
            raise RuntimeError("Checkout URL changed externally; left unchanged")
        sandbox.api("PATCH", "/settings/checkout-url", {"default_url": original_url})

    sandbox.cleanups.append(("restore default checkout URL", restore_url))
    sandbox.api("PATCH", "/settings/checkout-url", {"default_url": test_url})
    token = sandbox.api("POST", "/client-tokens", {"name": tag})
    sandbox.cleanups.append(("revoke temporary client-side token", lambda:
                             sandbox.api("PATCH", "/client-tokens/" + token["id"], {"status": "revoked"})))
    product = sandbox.api("POST", "/products", {"name": tag, "tax_category": "standard"})
    print("Product:", product["id"], flush=True)
    sandbox.cleanups.append(("archive product", lambda:
                             sandbox.api("PATCH", "/products/" + product["id"], {"status": "archived"})))
    price = sandbox.api("POST", "/prices", {
        "product_id": product["id"], "description": tag,
        "unit_price": {"amount": "1000", "currency_code": "GBP"},
        "billing_cycle": {"interval": "month", "frequency": 1}})
    print("Price:", price["id"], flush=True)
    sandbox.cleanups.append(("archive price", lambda:
                             sandbox.api("PATCH", "/prices/" + price["id"], {"status": "archived"})))
    address = sandbox.api("POST", "/customers/" + customer_id + "/addresses", {
        "first_line": "123 Main Street", "city": "London", "region": "Greater London",
        "postal_code": "SW1A 1AA", "country_code": "GB"})
    transaction = sandbox.api("POST", "/transactions", {
        "customer_id": customer_id, "address_id": address["id"], "collection_mode": "automatic",
        "items": [{"price_id": price["id"], "quantity": 1}],
        "custom_data": {"functional_test_run": tag}})
    transaction_id = transaction["id"]
    print("Transaction:", transaction_id, flush=True)

    def cancel_checkout():
        current = sandbox.api("GET", "/transactions/" + transaction_id)
        if current.get("subscription_id"):
            subscription_id = current["subscription_id"]
            subscription = sandbox.api("GET", "/subscriptions/" + subscription_id)
            if subscription["status"] != "canceled":
                sandbox.api("POST", "/subscriptions/" + subscription_id + "/cancel",
                            {"effective_from": "immediately"})
        if current["status"] in ("draft", "ready"):
            sandbox.api("PATCH", "/transactions/" + transaction_id, {"status": "canceled"})
        elif current["status"] != "completed":
            raise RuntimeError("Transaction requires inspection: " + transaction_id)

    sandbox.cleanups.append(("cancel checkout subscription or unpaid transaction", cancel_checkout))
    receiver.page = checkout_page(token["token"], transaction_id)
    print("CHECKOUT URL:", test_url, flush=True)
    print("Open this URL in a browser and complete the sandbox card form.", flush=True)

    def completed():
        current = sandbox.api("GET", "/transactions/" + transaction_id)
        return current["status"] == "completed" and current.get("subscription_id")

    wait_for("completed card transaction and subscription link", completed, args.checkout_wait)
    paid = sandbox.api("GET", "/transactions/" + transaction_id)
    if not any(p.get("status") == "captured" and p.get("method_details", {}).get("type") == "card"
               for p in paid.get("payments", [])):
        raise RuntimeError("Completed transaction has no captured card payment")
    subscription_id = paid["subscription_id"]
    print("Subscription:", subscription_id, flush=True)
    wait_for("active card subscription", lambda:
             sandbox.api("GET", "/subscriptions/" + subscription_id)["status"] == "active", 60)
    wait_for("signed transaction.completed delivery", lambda:
             receiver.received("transaction.completed", transaction_id), args.webhook_wait)
    wait_for("signed subscription.created delivery", lambda:
             receiver.received("subscription.created", subscription_id), args.webhook_wait)
    print("Card checkout and webhook tests passed. The sandbox paid transaction remains for inspection.", flush=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--public-url", required=True, help="Temporary HTTPS tunnel origin, no path")
    parser.add_argument("--port", type=int, default=8765)
    parser.add_argument("--webhook-only", action="store_true")
    parser.add_argument("--webhook-wait", type=int, default=120)
    parser.add_argument("--checkout-wait", type=int, default=600)
    parser.add_argument("--verifier", help="Precompiled BillingWebhookVerify executable")
    args = parser.parse_args()
    args.public_url = args.public_url.rstrip("/")
    parsed = urlsplit(args.public_url)
    if parsed.scheme != "https" or not parsed.netloc or parsed.path or parsed.query or parsed.fragment:
        parser.error("--public-url must be an HTTPS origin without a path")
    sandbox = Sandbox(os.environ.get("PADDLE_SANDBOX_API_KEY", ""))
    root = Path(__file__).resolve().parent.parent
    with tempfile.TemporaryDirectory(prefix="paddle-checkout-") as build:
        verifier = args.verifier
        if verifier is None:
            stack = shutil.which("stack")
            if stack is None:
                raise RuntimeError("Install Stack or supply --verifier")
            verifier = str(Path(build) / "verify-webhook")
            subprocess.run([stack, "--no-nix-pure", "exec", "--", "ghc", "-Wall", "-Werror",
                            "-package", "paddle", "-outputdir", build,
                            "test/BillingWebhookVerify.hs", "-o", verifier], cwd=root, check=True)
        receiver = Receiver(str(Path(verifier).resolve()))
        server = ThreadingHTTPServer(("127.0.0.1", args.port), receiver.handler())
        server.daemon_threads = True
        thread = threading.Thread(target=server.serve_forever, daemon=True)
        thread.start()
        ok = False
        try:
            # Verify the tunnel routes to this server before creating remote resources.
            with urlopen(args.public_url + "/health", timeout=30) as response:
                if response.read() != b"paddle-sandbox-harness":
                    raise RuntimeError("Public URL does not reach this harness")
            run(sandbox, receiver, args)
            ok = True
        finally:
            cleaned = sandbox.cleanup()
            server.shutdown()
            server.server_close()
            thread.join()
        if not (ok and cleaned):
            raise RuntimeError("Sandbox test or cleanup failed")


if __name__ == "__main__":
    try:
        main()
    except (RuntimeError, HTTPError, URLError, subprocess.CalledProcessError) as error:
        # HTTPError/URLError text can include a URL; only our sanitized errors are rendered.
        print("FAIL:", str(error) if isinstance(error, RuntimeError) else type(error).__name__, flush=True)
        raise SystemExit(1)
    except KeyboardInterrupt:
        print("Interrupted; cleanup attempted.", flush=True)
        raise SystemExit(130)
