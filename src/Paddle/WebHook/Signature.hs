module Paddle.WebHook.Signature where 

import Prelude ()
import qualified Prelude
import Protolude
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified OpenSSL as SSL
import qualified OpenSSL.EVP.Digest as SSL
import qualified OpenSSL.EVP.Verify as SSL
import qualified Data.ByteString.Base64 as Base64

import Paddle.Env (Env(..))

type SignatureBody = M.Map ByteString ByteString

-- Given all fields in webhook request, validate against their signature
validateSignature :: (MonadIO m) => Env -> SignatureBody -> m (Either Text ())
validateSignature env fields = do
  case signature >>= signatureDecode >>= serialize of
    Left err -> return $ Left (toS err)
    Right (serializedFields, sigBytes) -> liftIO $ SSL.withOpenSSL $ do
      Just sha1 <- SSL.getDigestByName "SHA1"
      verifyRes <- SSL.verifyBS sha1 sigBytes (pctxPubKey env) serializedFields

      if (verifyRes == SSL.VerifySuccess)
      then return $ Right ()
      else return $ Left "Request has invalid signature"
  where
    signature :: Either Prelude.String ByteString
    signature = maybeToEither "Missing signature field." (M.lookup "p_signature" fields)

    signatureDecode :: ByteString -> Either Prelude.String ByteString
    signatureDecode = Base64.decode . toS

    -- we need to verify the signature against the PHP-serialized request parameters (excluding p_signature)
    -- here's a gist that does it in Swift: https://gist.github.com/drewmccormack/a51b18ffeda8f596a11a8623481344d8
    serializeString str = "s:" <> BSB.intDec (BS.length str) <> ":\"" <> BSB.byteString str <> "\";"
    serialize sig = (\x -> Right (x, sig)) <$> LBS.toStrict $ BSB.toLazyByteString $
            "a:" <> BSB.intDec (M.size fields - 1) <> ":{" <>
              foldMap (\( key, value ) -> serializeString key <> serializeString value) (M.toAscList $ M.delete "p_signature" fields)
            <> "}"