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

-- | Parse an RSA public key from PEM-encoded text
parseRSAPublicKey :: Text -> Either Text RSA.PublicKey
parseRSAPublicKey pemText = do
  pems <- first (("PEM parse error: " <>) . toS) $ PEM.pemParseBS (encodeUtf8 pemText)
  pem <- case pems of
    [p] -> Right p
    []  -> Left "No PEM objects found"
    _   -> Left "Multiple PEM objects found"
  
  asn1 <- first (("ASN1 decode error: " <>) . show) $ 
    ASN1.decodeASN1' ASN1.DER (PEM.pemContent pem)
  
  case ASN1.fromASN1 asn1 of
    Right (X509.PubKeyRSA key, _) -> Right key
    Right _ -> Left "Public key is not RSA"
    Left err -> Left $ "ASN1 parse error: " <> show err

init :: (MonadIO m) => PaddleSecrets -> m (Either Text Env)
init secrets = do
  return $ do
    pubKey <- parseRSAPublicKey (paddlePublicKey secrets)
    return Env
      { pctxPubKey = pubKey
      , pctxVendorId = paddleVendorId secrets
      , pctxApiKey = paddleApiKey secrets
      }
