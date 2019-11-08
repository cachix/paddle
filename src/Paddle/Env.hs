module Paddle.Env where

import Prelude ()
import Protolude
import qualified OpenSSL as SSL
import qualified OpenSSL.EVP.PKey as SSL
import qualified OpenSSL.PEM as SSL

data PaddleSecrets = PaddleSecrets
  { paddlePublicKey :: Text -- ^ despite being a public key this must remain secret or anyone who has it can call our webhooks
  , paddleApiKey :: Text
  , paddleVendorId :: Int
  } deriving (Eq)

-- | Various keys for Paddle. These should all be kept secret.
data Env = Env
  { pctxPubKey :: SSL.SomePublicKey
  , pctxVendorId :: Int
  , pctxApiKey :: Text
  } deriving (Eq)

init :: (MonadIO m) => PaddleSecrets -> m Env
init secrets = do
  pubKey <- liftIO $ SSL.withOpenSSL $ SSL.readPublicKey (toS (paddlePublicKey secrets))
  return Env
    { pctxPubKey = pubKey
    , pctxVendorId = paddleVendorId secrets
    , pctxApiKey = paddleApiKey secrets
    }