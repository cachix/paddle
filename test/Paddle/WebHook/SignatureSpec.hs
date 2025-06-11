module Paddle.WebHook.SignatureSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Paddle.Env (Env (..), PaddleSecrets (..), init)
import Paddle.WebHook.Signature (SignatureBody, serializeFields, validateSignature)
import Protolude
import System.Process (readProcess)
import Test.Hspec
import Prelude ()

spec :: Spec
spec = describe "Paddle.WebHook.Signature" $ do
  describe "validateSignature" $ do
    it "validates a correct signature with real webhook data" $ do
      signedData <- generateSignedWebhookData
      pubKey <- getTestPublicKey
      env <- createTestEnv pubKey
      result <- validateSignature env signedData
      result `shouldSatisfy` isRight

    it "rejects an invalid signature" $ do
      let invalidData = M.fromList [("alert_name", ["test"]), ("p_signature", ["invalid_signature"])]
      pubKey <- getTestPublicKey
      env <- createTestEnv pubKey
      result <- validateSignature env invalidData
      result `shouldSatisfy` isLeft

-- Test webhook data (without signature)
testWebhookFields :: M.Map Text Text
testWebhookFields =
  M.fromList
    [ ("alert_name", "subscription_created"),
      ("cancel_url", "https://checkout.paddle.com/subscription/cancel?user=5&subscription=2&hash=ea8aec0ed511b91f58b44c5c7e519891de2e301e"),
      ("checkout_id", "2-d1bab8dbfc11d77-157eb37302"),
      ("currency", "USD"),
      ("email", "user@example.org"),
      ("event_time", "2021-10-30 19:55:06"),
      ("marketing_consent", ""),
      ("next_bill_date", "2021-11-05"),
      ("passthrough", "UserID"),
      ("quantity", "50"),
      ("status", "active"),
      ("subscription_id", "1"),
      ("subscription_plan_id", "4"),
      ("unit_price", "unit_price"),
      ("update_url", "https://checkout.paddle.com/subscription/update?user=7&subscription=6&hash=e88ee8ac9ef0ba6bd2fcdc577524030f02f14f67")
    ]

-- Read public key from file
getTestPublicKey :: IO Text
getTestPublicKey = readFile "test/test_public.pem"

-- Generate signed webhook data using the exact same process as validation
generateSignedWebhookData :: IO SignatureBody
generateSignedWebhookData = do
  -- Convert to ByteString map for serialization (same as validation function)
  let fieldsBS = M.fromList $ map (bimap encodeUtf8 encodeUtf8) (M.toList testWebhookFields)
  -- Serialize using the same function as validation
  let serializedFields = serializeFields fieldsBS
  -- Use openssl to sign the serialized fields
  signature <- signWithOpenssl serializedFields
  -- Create SignatureBody with signature included
  let signatureBody = M.fromList $ map (\(k, v) -> (k, [v])) (M.toList testWebhookFields) ++ [("p_signature", [signature])]
  return signatureBody

-- Use openssl command to sign data and return base64 encoded signature
signWithOpenssl :: BS.ByteString -> IO Text
signWithOpenssl dataToSign = do
  -- Write data to temp file, sign it, and read the signature
  let tempFile = "/tmp/paddle_test_data"
  BS.writeFile tempFile dataToSign
  -- Use openssl to sign and pipe to base64
  signatureOutput <- readProcess "sh" ["-c", "openssl dgst -sha1 -sign test/test_private.pem " ++ tempFile ++ " | base64 -w 0"] ""
  let signature = T.strip $ toS signatureOutput
  return signature

-- Helper function to create test environment
createTestEnv :: Text -> IO Env
createTestEnv pubKeyPem = do
  let secrets =
        PaddleSecrets
          { paddlePublicKey = pubKeyPem,
            paddleApiKey = "test-api-key",
            paddleVendorId = 12345
          }
  Paddle.Env.init secrets
