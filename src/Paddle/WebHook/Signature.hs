module Paddle.WebHook.Signature where

import qualified Crypto.Hash as Hash
import qualified Crypto.PubKey.RSA.PKCS15 as RSA
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as M
import Paddle.Env (Env (..))
import Protolude hiding (toS)
import Protolude.Conv
import Prelude ()
import qualified Prelude

type SignatureBody = M.Map Text [Text]

-- | Serialize a map of fields for signature verification
serializeFields :: M.Map ByteString ByteString -> ByteString
serializeFields fields =
  LBS.toStrict $
    BSB.toLazyByteString $
      "a:"
        <> BSB.intDec (M.size fields)
        <> ":{"
        <> foldMap serializePair (M.toAscList fields)
        <> "}"
  where
    serializePair (k, v) = serializeString k <> serializeString v
    serializeString s =
      "s:" <> BSB.intDec (BS.length s) <> ":\"" <> BSB.byteString s <> "\";"

-- Given all fields in webhook request, validate against their signature
validateSignature :: (MonadIO m) => Env -> SignatureBody -> m (Either Text ())
validateSignature env rawFields =
  let fields :: M.Map ByteString ByteString
      fields = M.fromList $ map (\(k, v) -> (toS k, toS (maybe "" identity (head v)))) (M.toList rawFields)

      signature :: Either Prelude.String ByteString
      signature = maybeToEither "Missing signature field." (M.lookup "p_signature" fields)

      signatureDecode :: ByteString -> Either Prelude.String ByteString
      signatureDecode = Base64.decode . toS

      -- we need to verify the signature against the PHP-serialized request parameters (excluding p_signature)
      -- here's a gist that does it in Swift: https://gist.github.com/drewmccormack/a51b18ffeda8f596a11a8623481344d8
      serialize sig = Right (serializeFields (M.delete "p_signature" fields), sig)
   in do
        case signature >>= signatureDecode >>= serialize of
          Left err -> return $ Left (toS err)
          Right (serializedFields, sigBytes) -> do
            if RSA.verify (Just Hash.SHA1) (pctxPubKey env) serializedFields sigBytes
              then return $ Right ()
              else return $ Left "Request has invalid signature"
