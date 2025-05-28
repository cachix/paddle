module Paddle.WebHook.Signature where

import qualified Crypto.Hash as Hash
import qualified Crypto.PubKey.RSA.PKCS15 as RSA
import qualified Data.ByteArray as BA
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

-- Given all fields in webhook request, validate against their signature
validateSignature :: (MonadIO m) => Env -> SignatureBody -> m (Either Text ())
validateSignature env rawFields =
  let fields = M.fromList $ map (\(k, v) -> (toS k, toS (maybe "" identity (head v)))) (M.toList rawFields)

      signature :: Either Prelude.String ByteString
      signature = maybeToEither "Missing signature field." (M.lookup "p_signature" fields)

      signatureDecode :: ByteString -> Either Prelude.String ByteString
      signatureDecode = Base64.decode . toS

      -- we need to verify the signature against the PHP-serialized request parameters (excluding p_signature)
      -- here's a gist that does it in Swift: https://gist.github.com/drewmccormack/a51b18ffeda8f596a11a8623481344d8
      serializeString str = "s:" <> BSB.intDec (BS.length str) <> ":\"" <> BSB.byteString str <> "\";"
      serialize sig =
        (\x -> Right (x, sig)) <$> LBS.toStrict $
          BSB.toLazyByteString $
            "a:"
              <> BSB.intDec (M.size fields - 1)
              <> ":{"
              <> foldMap (\(key, value) -> serializeString key <> serializeString value) (M.toAscList $ M.delete "p_signature" fields)
              <> "}"
   in do
        case signature >>= signatureDecode >>= serialize of
          Left err -> return $ Left (toS err)
          Right (serializedFields, sigBytes) -> do
            let sha1Hash = Hash.hash serializedFields :: Hash.Digest Hash.SHA1
            case RSA.verify (Just Hash.SHA1) (pctxPubKey env) (BA.convert sha1Hash) sigBytes of
              True -> return $ Right ()
              False -> return $ Left "Request has invalid signature"
