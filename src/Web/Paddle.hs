{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Web.Paddle
  ( Context()
  , Web.Paddle.init
  , PaddleProductId(..)
  , PaddleCheckoutId(..)
  , FulfillmentWebhookRequest(..)
  , parseFulfillmentWebhookRequest
  , PaddleOrderInfo(..)
  , getOrderInfo
  , PaddleTransaction(..)
  , PaddleTransactionStatus(..)
  , getTransactionsForCheckout
  ) where

import           Control.Monad (unless)
import           Data.Aeson (eitherDecode)
import           Data.Aeson.Types
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Hashable (Hashable)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Read as TR
import           Data.Traversable (for)
import           Network.HTTP.Simple hiding (Proxy)
import qualified OpenSSL as SSL
import qualified OpenSSL.EVP.Digest as SSL
import qualified OpenSSL.EVP.PKey as SSL
import qualified OpenSSL.EVP.Verify as SSL
import qualified OpenSSL.PEM as SSL
import           UnliftIO

type MultiPartParam = (ByteString, ByteString)

newtype PaddleProductId = PaddleProductId { unPaddleProductId :: Int }
  deriving (Eq, Ord, Show, Hashable)

newtype PaddleCheckoutId = PaddleCheckoutId { unPaddleCheckoutId :: Text }
  deriving (Eq, Ord, Show)

data PaddleSecrets = PaddleSecrets
  { paddlePublicKey :: Text -- ^ despite being a public key this must remain secret or anyone who has it can call our webhooks
  , paddleApiKey :: Text
  , paddleVendorId :: Int
  } deriving (Eq)

-- | Various keys for Paddle. These should all be kept secret.
data Context = Context
  { pctxPubKey :: SSL.SomePublicKey
  , pctxVendorId :: Int
  , pctxApiKey :: Text
  } deriving (Eq)

init :: (MonadIO m) => PaddleSecrets -> m Context
init secrets = do
  pctxPubKey <- liftIO $ SSL.withOpenSSL $ SSL.readPublicKey (T.unpack (paddlePublicKey secrets))
  return Context
    { pctxPubKey
    , pctxVendorId = paddleVendorId secrets
    , pctxApiKey = paddleApiKey secrets
    }

data FulfillmentWebhookRequest = FulfillmentWebhookRequest
  { fwrProductId :: PaddleProductId
  , fwrPassthrough :: ByteString
  } deriving (Eq, Ord, Show)

newtype PaddleException = PaddleException Text
  deriving (Eq, Ord, Show)

instance Exception PaddleException

parseFulfillmentWebhookRequest :: (MonadIO m) => Context -> [MultiPartParam] -> m FulfillmentWebhookRequest
parseFulfillmentWebhookRequest ctx fieldList = do
  let fields = M.fromList fieldList
  let getField name = case M.lookup name fields of
        Nothing -> throwIO $ PaddleException $ "Expected field " <> T.decodeUtf8With T.lenientDecode name
        Just val -> return val
  -- verify that the webhook request is from Paddle
  -- https://paddle.com/docs/reference-verifying-webhooks/
  sig <- getField "p_signature"
  sigBytes <- case Base64.decode sig of
    Left err -> throwIO $ PaddleException $ "Could not decode signature: " <> T.pack err
    Right sigBytes -> return sigBytes
  -- we need to verify the signature against the PHP-serialized request parameters (excluding p_signature)
  -- here's a gist that does it in Swift: https://gist.github.com/drewmccormack/a51b18ffeda8f596a11a8623481344d8
  let serializeString str = "s:" <> BSB.intDec (BS.length str) <> ":\"" <> BSB.byteString str <> "\";"
  let serializedFields = LBS.toStrict $ BSB.toLazyByteString $
        "a:" <> BSB.intDec (M.size fields - 1) <> ":{" <>
          M.foldMapWithKey (\key value ->
            if key == "p_signature"
              then mempty
              else serializeString key <> serializeString value
            ) fields
        <> "}"
  verifyRes <- liftIO $ SSL.withOpenSSL $ do
    Just sha1 <- SSL.getDigestByName "SHA1"
    SSL.verifyBS sha1 sigBytes (pctxPubKey ctx) serializedFields
  unless (verifyRes == SSL.VerifySuccess) $
    throwIO $ PaddleException $ "Request has invalid signature"

  let parseNat val = case T.decodeUtf8' val of
        Left err -> throwIO $ PaddleException $ "Could not decode UTF-8: " <> T.pack (show err)
        Right numTxt -> case TR.decimal numTxt of
          Right ( n, "" ) -> return n
          _ -> throwIO $ PaddleException $ "Could not parse number: " <> numTxt

  fwrProductId <- (fmap PaddleProductId . parseNat) =<< getField "p_product_id"
  fwrPassthrough <- getField "passthrough"
  return $ FulfillmentWebhookRequest { fwrProductId, fwrPassthrough }

data PaddleOrderInfo = PaddleOrderInfo
  { poiProcessed :: Bool
  , poiLockers :: Maybe [ PaddleProductId ]
  } deriving (Eq, Ord, Show)

getOrderInfo :: (MonadIO m) => PaddleCheckoutId -> m PaddleOrderInfo
getOrderInfo checkoutId = do
  req <- liftIO $ parseRequest $ T.unpack ("https://checkout.paddle.com/api/1.0/order?checkout_id=" <> unPaddleCheckoutId checkoutId)
  result <- getResponseBody <$> httpLBS req
  let parseResult = withObject "order info" $ \info -> do
        state :: Text <- info .: "state"
         -- we don't know what the other values are from the documentation
        let poiProcessed = state == "processed"
        mbLockers <- info .:? "lockers"
        poiLockers <- for mbLockers $ \lockers ->
          for lockers $ \locker ->
            PaddleProductId <$> (locker .: "product_id")
        return PaddleOrderInfo { poiProcessed, poiLockers }
  case eitherDecode result >>= parseEither parseResult of
    Left err -> throwIO $ PaddleException $ "Failed to parse order info: " <> T.pack err
    Right orderInfo -> return orderInfo

data PaddleTransactionStatus
  = PTSCompleted
  | PTSRefunded
     deriving (Eq, Ord, Show)

data PaddleTransaction = PaddleTransaction
  { ptPassthrough :: Maybe Text
  , ptStatus :: PaddleTransactionStatus
  } deriving (Eq, Ord, Show)

getTransactionsForCheckout :: (MonadIO m) => Context -> PaddleCheckoutId -> m [ PaddleTransaction ]
getTransactionsForCheckout ctx checkoutId = do
  req <- liftIO $ parseRequest $ T.unpack ("https://vendors.paddle.com/api/2.0/checkout/" <> unPaddleCheckoutId checkoutId <> "/transactions")
  let reqWithBody = setRequestBodyURLEncoded
        [ ( "vendor_id", T.encodeUtf8 (T.pack $ show (pctxVendorId ctx :: Int)) )
        , ( "vendor_auth_code", T.encodeUtf8 (pctxApiKey ctx) )
        ] req
  result <- getResponseBody <$> httpLBS reqWithBody
  let parseRes = withObject "transactions" $ \transObj -> do
        success <- transObj .: "success"
        if success
          then do
            transList <- transObj .: "response"
            transactions <- for transList $ \trans -> do
              statusTxt :: Text <- trans .: "status"
              ptStatus <- case statusTxt of
                "completed" -> return PTSCompleted
                "refunded" -> return PTSRefunded
                _ -> fail $ "Unrecognized transaction status: " <> T.unpack statusTxt
              ptPassthrough <- trans .:? "passthrough"
              return PaddleTransaction { ptStatus, ptPassthrough }
            return (Right transactions)
          else do
            msg :: Text <- (transObj .: "error") >>= \err -> err .: "message"
            return (Left msg)
  case eitherDecode result >>= parseEither parseRes of
    Left parseErr -> throwIO $ PaddleException $ "Could not parse transaction list: " <> T.pack parseErr
    Right (Left err) -> throwIO $ PaddleException $ "Could not fetch transaction list, paddle returned error: " <> err
    Right (Right res) -> return res
