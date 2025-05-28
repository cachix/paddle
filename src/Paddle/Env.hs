module Paddle.Env where

import Prelude ()
import Protolude
import qualified Data.PEM as PEM
import qualified Data.X509 as X509
import qualified Data.ASN1.BinaryEncoding as ASN1
import qualified Data.ASN1.Encoding as ASN1
import qualified Data.ASN1.Types as ASN1
import qualified Crypto.PubKey.RSA as RSA

data PaddleSecrets = PaddleSecrets
  { paddlePublicKey :: Text -- ^ despite being a public key this must remain secret or anyone who has it can call our webhooks
  , paddleApiKey :: Text
  , paddleVendorId :: Int
  } deriving (Eq)

-- | Various keys for Paddle. These should all be kept secret.
data Env = Env
  { pctxPubKey :: RSA.PublicKey
  , pctxVendorId :: Int
  , pctxApiKey :: Text
  } deriving (Eq)

init :: (MonadIO m) => PaddleSecrets -> m Env
init secrets = do
  pubKey <- case PEM.pemParseBS (encodeUtf8 (paddlePublicKey secrets)) of
    Right [pem] -> case ASN1.decodeASN1' ASN1.DER (PEM.pemContent pem) of
      Right asn1 -> case ASN1.fromASN1 asn1 of
        Right (X509.PubKeyRSA rsaKey, _) -> return rsaKey
        Right _ -> panic "Public key is not RSA"
        Left err -> panic $ "Failed to parse public key from ASN1: " <> show err
      Left err -> panic $ "Failed to decode ASN1: " <> show err
    Right [] -> panic "No PEM objects found"
    Right _ -> panic "Multiple PEM objects found, expected one"
    Left err -> panic $ "Failed to parse PEM: " <> show err
  return Env
    { pctxPubKey = pubKey
    , pctxVendorId = paddleVendorId secrets
    , pctxApiKey = paddleApiKey secrets
    }