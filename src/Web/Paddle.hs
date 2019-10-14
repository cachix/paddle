{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Web.Paddle
  ( Context()
  , Web.Paddle.init
  , PaddleProductId(..)
  , PaddleCheckoutId(..)
  , PaddleSecrets(..)
  , FulfillmentWebhookRequest(..)
  , parseFulfillmentWebhookRequest
  , AlertWebhookRequest(..)

  , Passthrough(..)
  , SubscriptionPlanId(..)
  , SubscriptionId(..)
  , NextBillDate(..)
  , UpdateUrl(..)
  , CancellationEffectiveDate(..)

  , SubscriptionCreated
  , SubscriptionCancelled

  , IsEvent()
  , eventField

  , parseAlertWebhookRequest
  , PaddleOrderInfo(..)
  , getOrderInfo
  , PaddleTransaction(..)
  , PaddleTransactionStatus(..)
  , getTransactionsForCheckout

  , UpdateSubscription(..)
  , updateSubscription
  , cancelSubscription

  , PaddleCustomCheckout(..)
  , generatePayLink
  ) where

import           Control.Monad (unless)
import           Data.Aeson (encode, eitherDecode)
import           Data.Aeson.Types hiding (parseField)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Fixed (Centi, showFixed)
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Data.Time
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Read as TR
import           Data.Traversable (for)
import           Data.Typeable
import           GHC.Generics
import           Network.HTTP.Simple hiding (Proxy)
import qualified OpenSSL as SSL
import qualified OpenSSL.EVP.Digest as SSL
import qualified OpenSSL.EVP.PKey as SSL
import qualified OpenSSL.EVP.Verify as SSL
import qualified OpenSSL.PEM as SSL
import           UnliftIO

type MultiPartParam = (ByteString, ByteString)

data Currency =
    CurrUsd
  deriving (Eq, Ord, Show, Generic)
instance Hashable Currency

-- | We define this type solely to have types not relying on JSON numbers,
-- since they'd have to go through Double when decoding on the browser.
newtype Money = Money {unMoney :: Centi}
  deriving (Eq, Show, Ord, Num, Fractional)

{-# INLINE tshow #-}
tshow :: Show a => a -> Text
tshow = T.pack . show

type PaymentId = Text
type UserId = Text

-- This is a type we can serialize and attach to Paddle payments so we know what they were for.
data PaddlePassthrough
  = PPPayment PaymentId
  | PPSubscription UserId
  deriving (Eq, Show, Generic)

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
  { fwrPassthrough :: BS.ByteString
  }
  deriving (Eq, Ord, Show)

newtype PaddleException = PaddleException Text
  deriving (Eq, Ord, Show)

instance Exception PaddleException

validateRequest :: (MonadIO m) => Context -> [ MultiPartParam ] -> m (M.Map ByteString ByteString)
validateRequest ctx fieldList = do
  let fields = M.fromList fieldList
  ( sig, otherFields ) <- case M.splitLookup "p_signature" fields of
    ( less, Just sig, more ) -> return ( sig, M.toAscList less <> M.toAscList more )
    _ -> throwIO $ PaddleException "Missing signature field."
  sigBytes <- case Base64.decode sig of
    Left err -> throwIO $ PaddleException $ "Could not decode signature: " <> T.pack err
    Right sigBytes -> return sigBytes
  -- we need to verify the signature against the PHP-serialized request parameters (excluding p_signature)
  -- here's a gist that does it in Swift: https://gist.github.com/drewmccormack/a51b18ffeda8f596a11a8623481344d8
  let serializeString str = "s:" <> BSB.intDec (BS.length str) <> ":\"" <> BSB.byteString str <> "\";"
  let serializedFields = LBS.toStrict $ BSB.toLazyByteString $
        "a:" <> BSB.intDec (M.size fields - 1) <> ":{" <>
          foldMap (\( key, value ) -> serializeString key <> serializeString value) otherFields
        <> "}"
  verifyRes <- liftIO $ SSL.withOpenSSL $ do
    Just sha1 <- SSL.getDigestByName "SHA1"
    SSL.verifyBS sha1 sigBytes (pctxPubKey ctx) serializedFields
  unless (verifyRes == SSL.VerifySuccess) $
    throwIO $ PaddleException $ "Request has invalid signature"

  return fields

data PaddleField where
  PaddleField :: (Show a, Typeable a) => a -> PaddleField

deriving instance Show PaddleField

newtype PaddleEvent = PaddleEvent (HMS.HashMap TypeRep PaddleField)
  deriving (Show)

instance IsEvent PaddleEvent where
  toPaddleEvent = id
  fromPaddleEvent = id

type family HasField fields field where
  HasField '[] field = 'False
  HasField (field ': fields) field = 'True
  HasField (otherField ': fields) field = HasField fields field

type family EventFields event :: [*]

class (Show field, Typeable field) => IsField field where
  parseField :: (MonadIO m) => ( ByteString, ByteString -> m field )

class ParseFields (fields :: [*]) where
  parseFields :: (MonadIO m) => Fields -> Proxy fields -> m [ PaddleField ]

instance ParseFields '[] where
  parseFields _ _ = pure []

instance (IsField x, ParseFields xs) => ParseFields (x ': xs) where
  parseFields fields _ = do
    let ( name, parseVal ) = parseField
    valBS <- getField fields name
    val :: x <- parseVal valBS
    rest <- parseFields fields (Proxy @xs)
    return (PaddleField val : rest)

class IsEvent event where
  toPaddleEvent :: event -> PaddleEvent
  fromPaddleEvent :: PaddleEvent -> event

eventField :: forall field event. (IsEvent event, HasField (EventFields event) field ~ 'True, Typeable field) => event -> field
eventField ev = fromMaybe (error "Event did not contain expected field.") $ do
  let PaddleEvent fields = toPaddleEvent ev
  PaddleField val <- HMS.lookup (typeRep (Proxy @field)) fields
  cast val

natField :: (MonadIO m) => ByteString -> (Int -> field) -> ( ByteString, ByteString -> m field )
natField name f = ( name, \bs -> f <$> parseNat bs )

textField :: (MonadIO m) => ByteString -> (Text -> field) -> ( ByteString, ByteString -> m field )
textField name f = ( name, \bs -> pure (f (T.decodeUtf8 bs)) )

dateField :: (MonadIO m) => ByteString -> (Day -> field) -> ( ByteString, ByteString -> m field )
dateField name f =
  ( name
  , \bs -> f <$> parseTimeM True defaultTimeLocale "%Y-%-m-%-d" (T.unpack (T.decodeUtf8 bs))
  )

newtype Passthrough = Passthrough { unPassthrough :: BS.ByteString } deriving (Eq, Ord, Show)
instance IsField Passthrough where parseField = ( "passthrough", pure . Passthrough )

newtype SubscriptionPlanId = SubscriptionPlanId { unSubscriptionPlanId :: PaddleProductId } deriving (Eq, Ord, Show)
instance IsField SubscriptionPlanId where parseField = natField "subscription_plan_id" (SubscriptionPlanId . PaddleProductId)

newtype SubscriptionId = SubscriptionId { unSubscriptionId :: Int } deriving (Eq, Ord, Show, FromJSON, ToJSON)
instance IsField SubscriptionId where parseField = natField "subscription_id" SubscriptionId

newtype NextBillDate = NextBillDate { unNextBillDate :: Day } deriving (Eq, Ord, Show)
instance IsField NextBillDate where parseField = dateField "next_bill_date" NextBillDate

newtype CancellationEffectiveDate = CancellationEffectiveDate { unCancellationEffectiveDate :: Day } deriving (Eq, Ord, Show)
instance IsField CancellationEffectiveDate where parseField = dateField "cancellation_effective_date" CancellationEffectiveDate

newtype UpdateUrl = UpdateUrl { unUpdateUrl :: Text } deriving (Eq, Ord, Show, FromJSON, ToJSON)
instance IsField UpdateUrl where parseField = textField "update_url" UpdateUrl

newtype SubscriptionCreated = SubscriptionCreated PaddleEvent deriving (Show, IsEvent)
type instance EventFields SubscriptionCreated =
  [ SubscriptionPlanId
  , Passthrough
  , SubscriptionId
  , NextBillDate
  , UpdateUrl
  ]

newtype SubscriptionCancelled = SubscriptionCancelled PaddleEvent deriving (Show, IsEvent)
type instance EventFields SubscriptionCancelled =
  [ SubscriptionPlanId
  , Passthrough
  , SubscriptionId
  , CancellationEffectiveDate
  ]

newtype SubscriptionUpdated = SubscriptionUpdated PaddleEvent deriving (Show, IsEvent)
type instance EventFields SubscriptionUpdated =
  [ SubscriptionPlanId
  , Passthrough
  , SubscriptionId
  , NextBillDate
  , UpdateUrl
  ]

newtype SubscriptionPaymentSucceeded = SubscriptionPaymentSucceeded PaddleEvent deriving (Show, IsEvent)
type instance EventFields SubscriptionPaymentSucceeded =
  [ SubscriptionPlanId
  , Passthrough
  ]

data AlertWebhookRequest
  = AWRSubscriptionCreated SubscriptionCreated
  | AWRSubscriptionUpdated SubscriptionUpdated
  | AWRSubscriptionCancelled SubscriptionCancelled
  | AWRSubscriptionPaymentSucceeded SubscriptionPaymentSucceeded
  | AWRSubscriptionPaymentFailed
  | AWRSubscriptionPaymentRefunded
  | AWRLockerProcessed
  | AWRPaymentSucceeded
  | AWRPaymentRefunded
  | AWRHighRiskTransactionCreated
  | AWRHighRiskTransactionUpdated
  | AWRPaymentDisputeCreated
  | AWRPaymentDisputeClosed
  | AWRTransferCreated
  | AWRTransferPaid
  | AWRNewAudienceMember
  | AWRUpdateAudienceMember
    deriving (Show)

type Fields = M.Map ByteString ByteString

getField :: (MonadIO m) => Fields -> ByteString -> m ByteString
getField fields name = case M.lookup name fields of
  Nothing -> throwIO $ PaddleException $ "Expected field " <> T.decodeUtf8 name
  Just val -> return val

parseNat :: (MonadIO m) => ByteString -> m Int
parseNat val = case T.decodeUtf8' val of
  Left err -> throwIO $ PaddleException $ "Could not decode UTF-8: " <> T.pack (show err)
  Right numTxt -> case TR.decimal numTxt of
    Right ( n, "" ) -> return n
    _ -> throwIO $ PaddleException $ "Could not parse number: " <> numTxt

parseAlertWebhookRequest :: forall m. (MonadIO m) => Context -> [MultiPartParam] -> m AlertWebhookRequest
parseAlertWebhookRequest ctx fieldList = do
  fields <- validateRequest ctx fieldList

  let
    parseEvent :: forall event. (ParseFields (EventFields event), IsEvent event) => (event -> AlertWebhookRequest) -> (PaddleEvent -> event) -> m AlertWebhookRequest
    parseEvent f g = do
      paddleFields <- parseFields fields (Proxy @(EventFields event))
      return $ f $ g $ PaddleEvent $ HMS.fromList $ map (\field@(PaddleField val) -> ( typeOf val, field)) paddleFields

  alertName <- getField fields "alert_name"
  case alertName of
    "subscription_created" -> parseEvent AWRSubscriptionCreated SubscriptionCreated
    "subscription_updated" -> parseEvent AWRSubscriptionUpdated SubscriptionUpdated
    "subscription_cancelled" -> parseEvent AWRSubscriptionCancelled SubscriptionCancelled
    "subscription_payment_succeeded" -> parseEvent AWRSubscriptionPaymentSucceeded SubscriptionPaymentSucceeded
    "subscription_payment_failed" -> pure AWRSubscriptionPaymentFailed {}
    "subscription_payment_refunded" -> pure AWRSubscriptionPaymentRefunded {}
    "locker_processed" -> pure AWRLockerProcessed {}
    "payment_succeeded" -> pure AWRPaymentSucceeded {}
    "payment_refunded" -> pure AWRPaymentRefunded {}
    "high_risk_transaction_created" -> pure AWRHighRiskTransactionCreated {}
    "high_risk_transaction_updated" -> pure AWRHighRiskTransactionUpdated {}
    "payment_dispute_created" -> pure AWRPaymentDisputeCreated {}
    "payment_dispute_closed" -> pure AWRPaymentDisputeClosed {}
    "transfer_created" -> pure AWRTransferCreated {}
    "transfer_paid" -> pure AWRTransferPaid {}
    "new_audience_member" -> pure AWRNewAudienceMember {}
    "update_audience_member" -> pure AWRUpdateAudienceMember {}
    _ -> throwIO $ PaddleException $ "Unexpected Paddle alert: " <> T.decodeUtf8 alertName

parseFulfillmentWebhookRequest :: forall m. (MonadIO m) => Context -> [MultiPartParam] -> m FulfillmentWebhookRequest
parseFulfillmentWebhookRequest ctx fieldList = do
  fields <- validateRequest ctx fieldList

  fwrPassthrough <- getField fields "passthrough"
  return $ FulfillmentWebhookRequest { fwrPassthrough }

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

paddleRequest :: (MonadIO m, FromJSON response) => Context -> String -> [ ( ByteString, ByteString ) ] -> (response -> Parser a) -> m a
paddleRequest ctx url params responseParser = do
  req <- liftIO $ parseRequest url
  let reqParams =
        [ ( "vendor_id", encodeUtf8 (T.pack $ show (pctxVendorId ctx :: Int)) )
        , ( "vendor_auth_code", encodeUtf8 (pctxApiKey ctx) )
        ] ++ params
  let reqWithBody = setRequestBodyURLEncoded reqParams req
  -- logInfo $ "Sending request to " <> url <> " with fields: " <> show reqParams
  resultLbs <- getResponseBody <$> httpLBS reqWithBody
  let parseRes resultVal = do
        responseObj <- parseJSON resultVal
        success <- responseObj .: "success"
        if success
          then do
            resp <- responseObj .: "response"
            Right <$> responseParser resp
          else do
            err <- responseObj .: "error"
            msg :: Text <- err .: "message"
            return (Left msg)
  case eitherDecode resultLbs >>= parseEither parseRes of
    Left parseErr -> throwIO $ PaddleException $ "Could not parse transaction list: " <> T.pack parseErr
    Right (Left err) -> throwIO $ PaddleException $ "Could not fetch transaction list, paddle returned error: " <> err
    Right (Right res) -> return res

getTransactionsForCheckout :: (MonadIO m) => Context -> PaddleCheckoutId -> m [ PaddleTransaction ]
getTransactionsForCheckout ctx checkoutId =
  paddleRequest
    ctx
    (T.unpack $ "https://vendors.paddle.com/api/2.0/checkout/" <> unPaddleCheckoutId checkoutId <> "/transactions")
    []
    $ \transList ->
      for transList $ \trans -> do
        statusTxt :: Text <- trans .: "status"
        ptStatus <- case statusTxt of
          "completed" -> return PTSCompleted
          "refunded" -> return PTSRefunded
          _ -> fail $ "Unrecognized transaction status: " <> T.unpack statusTxt
        ptPassthrough <- trans .:? "passthrough"
        return PaddleTransaction { ptStatus, ptPassthrough }

data UpdateSubscription = UpdateSubscription
  { usSubscriptionId :: SubscriptionId
  , usQuantity :: Int
  , usPlanId :: Maybe PaddleProductId
  , usBillImmediately :: Bool
  , usProrate :: Bool
  , usKeepModifiers :: Bool
  } deriving (Eq, Ord, Show)

boolParam :: Bool -> ByteString
boolParam b = if b then "1" else "0"

ignoreRes :: Value -> Parser ()
ignoreRes _ = return ()

updateSubscription :: (MonadIO m) => Context -> UpdateSubscription -> m ()
updateSubscription ctx update =
  paddleRequest
    ctx
    "https://vendors.paddle.com/api/2.0/subscription/users/update"
    (
      [ ( "subscription_id", encodeUtf8 (tshow (unSubscriptionId (usSubscriptionId update)) ))
      , ( "quantity", encodeUtf8 (tshow (usQuantity update)))
      , ( "bill_immediately", boolParam (usBillImmediately update) )
      , ( "prorate", boolParam (usProrate update) )
      , ( "keep_modifiers", boolParam (usKeepModifiers update) )
      ] ++ case usPlanId update of
        Nothing -> []
        Just planId -> [ ( "plan_id", encodeUtf8 (tshow (unPaddleProductId planId))) ]
    )
    ignoreRes

cancelSubscription :: (MonadIO m) => Context -> SubscriptionId -> m ()
cancelSubscription ctx subscriptionId =
  paddleRequest
    ctx
    "https://vendors.paddle.com/api/2.0/subscription/users_cancel"
    [ ( "subscription_id", encodeUtf8 (tshow (unSubscriptionId subscriptionId) )) ]
    ignoreRes

data PaddleCustomCheckout passthrough = PaddleCustomCheckout
  { pccTitle :: Text
  , pccWebhookUrl :: Text
  , pccPrices :: HMS.HashMap Currency Money
  , pccQuantityVariable :: Bool
  , pccCustomerEmail :: Text
  , pccPassthrough :: passthrough
  }

generatePayLink :: (MonadIO m, ToJSON passthrough) => Context -> PaddleCustomCheckout passthrough -> m Text
generatePayLink ctx checkout = do
  let prices = do
        ( priceIndex, ( curr, price ) ) <- zip [0..] (HMS.toList (pccPrices checkout))
        let currencyStr = case curr of
              CurrUsd -> "USD"
        return
          ( encodeUtf8 ("prices[" <> tshow (priceIndex :: Int) <> "]")
          , encodeUtf8 (currencyStr <> ":" <> T.pack (showFixed True (unMoney price)))
          )
  paddleRequest
    ctx
    "https://vendors.paddle.com/api/2.0/product/generate_pay_link"
    (
      [ ( "title", encodeUtf8 (pccTitle checkout) )
      , ( "webhook_url", encodeUtf8 (pccWebhookUrl checkout) )
      , ( "quantity_variable", boolParam (pccQuantityVariable checkout) )
      , ( "customer_email", encodeUtf8 (pccCustomerEmail checkout) )
      , ( "passthrough", LBS.toStrict (encode (pccPassthrough checkout)) )
      ] ++ prices
    )
    $ \response -> response .: "url"
