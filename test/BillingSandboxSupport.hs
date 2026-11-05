{-# LANGUAGE OverloadedStrings #-}
module BillingSandboxSupport
  ( check, loadSandboxKey, pollUntil, sandboxRequest, withCleanups, describeError ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, mask, throwIO, try)
import Control.Monad (unless)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Client (Manager)
import Network.HTTP.Types.Status (statusCode)
import qualified Paddle.Billing.Client as Billing
import Paddle.Billing.Types
import Servant.Client (ClientError(..), ClientM, responseStatusCode)
import System.Environment (lookupEnv)

loadSandboxKey :: IO Text
loadSandboxKey = do
  token <- Text.strip . maybe "" Text.pack <$> lookupEnv "PADDLE_SANDBOX_API_KEY"
  check "PADDLE_SANDBOX_API_KEY must contain a sandbox API key"
    ("pdl_sdbx_apikey_" `Text.isPrefixOf` token)
  pure token

check :: String -> Bool -> IO ()
check label condition = unless condition (fail ("Assertion failed: " <> label))

-- Only repeat reads while waiting for Paddle's asynchronous state transitions.
-- Writes are never retried: a lost response must not create duplicate records.
pollUntil :: String -> IO a -> (a -> Bool) -> IO a
pollUntil label action ready = go (30 :: Int)
  where
    go remaining = do
      value <- action
      if ready value then pure value
      else if remaining <= 1 then fail ("Timed out waiting for " <> label)
      else threadDelay 1000000 >> go (remaining - 1)

sandboxRequest :: Manager -> String -> ClientM (Response a) -> IO a
sandboxRequest manager label action = do
  result <- Billing.runClient manager Billing.Sandbox action
  case result of
    Left err -> fail (label <> ": " <> describeError err)
    Right response -> do
      check (label <> " has a request ID") (not (Text.null (metaRequestId (responseMeta response))))
      putStrLn ("PASS " <> label)
      pure (responseData response)

-- Run cleanup in reverse creation order, even after assertions fail. Attempt
-- every cleanup action and fail the suite if cleanup fails on a successful run.
withCleanups :: ((String -> IO () -> IO ()) -> IO ()) -> IO ()
withCleanups body = mask $ \restore -> do
  actions <- newIORef []
  result <- try (restore (body (\label action -> modifyIORef' actions ((label, action) :))))
  registered <- readIORef actions
  outcomes <- mapM (\(label, action) -> do
    outcome <- try action :: IO (Either SomeException ())
    case outcome of
      Left _ -> putStrLn ("FAIL cleanup: " <> label <> "; inspect the printed record IDs.")
      Right () -> putStrLn ("PASS cleanup: " <> label)
    pure outcome) registered
  case result of
    Left err -> throwIO (err :: SomeException)
    Right () -> do
      check "all cleanup actions succeeded" (all (either (const False) (const True)) outcomes)
      putStrLn "Sandbox tests passed. Test records remain for inspection."

-- Servant errors can contain Authorization headers and portal URL tokens.
-- Report the error category, HTTP status, and Paddle request ID without dumping
-- the underlying request or response.
describeError :: Billing.BillingClientError -> String
describeError (Billing.BillingApiError response _) =
  "Paddle error " <> Text.unpack (apiErrorCode (errorResponseError response))
  <> " (request " <> Text.unpack (metaRequestId (errorResponseMeta response)) <> ")"
describeError (Billing.BillingTransportError err) = case err of
  FailureResponse _ response -> "HTTP " <> show (statusCode (responseStatusCode response))
  DecodeFailure _ _ -> "response JSON did not match the Billing schema"
  UnsupportedContentType _ _ -> "unexpected response content type"
  InvalidContentTypeHeader _ -> "invalid response content type"
  ConnectionError _ -> "connection or TLS failure"
