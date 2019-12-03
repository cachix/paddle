{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Paddle.WebHook where


import           Protolude
import           Prelude ()
import           Data.Proxy                     ( Proxy(..) )
import           Data.Char                      (toLower, isUpper)
import           Servant.API
import           Web.FormUrlEncoded
import Paddle.WebHook.Signature (SignatureBody)

{-
data SubscriptionStatus =
    Active | Trialing | PastDue | Deleted
    deriving (Show, Eq)

instance FromHttpApiData SubscriptionStatus where
  where
    parseUrlPiece = 

instance FromForm SubscriptionStatus where
  fromForm form = 
    Left (trace ("form" :: Text) $ show form) -- TODO
-}

data SubscriptionCreated passthrough = SubscriptionCreated
  { subscriptionId :: Text
  , subscriptionPlanId :: Text
  , updateUrl :: Text
  , cancelUrl :: Text
  --, status :: SubscriptionStatus
  , passthrough :: passthrough
  } deriving (Generic, Show)

data PaddleWebHook passthrough 
  = SubscriptionCreatedWebHook (SubscriptionCreated passthrough)
  | UnknownWebHook Text
  deriving (Generic, Show)

-- TODO: validate request body using http://hackage.haskell.org/package/servant-0.16.2/docs/Servant-API-WithNamedContext.html
instance FromHttpApiData passthrough => FromForm (PaddleWebHook passthrough, SignatureBody) where
  fromForm form = 
    lookupUnique "alert_name" form >>= (\name -> liftA2 (,) (toWebHook name) signatureBody)
    where
      signatureBody :: Either Text SignatureBody
      signatureBody = fromForm form

      formOptions :: FormOptions
      formOptions =
        defaultFormOptions 
          { fieldLabelModifier = concatMap (\x -> if isUpper x then ['_', toLower x] else [x])
          }

      toWebHook :: Text -> Either Text (PaddleWebHook passthrough)
      toWebHook "subscription_created" = SubscriptionCreatedWebHook <$> genericFromForm formOptions form
      toWebHook name = Right $ UnknownWebHook name

type API passthrough
    = ReqBody '[FormUrlEncoded] (PaddleWebHook passthrough, SignatureBody)
    :> Post '[JSON] NoContent

api :: Proxy (API passthrough)
api = Proxy
