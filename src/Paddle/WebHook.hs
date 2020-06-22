{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Paddle.WebHook where


import           Protolude
import           Prelude ()
import           Servant.API
import           Web.FormUrlEncoded
import Paddle.WebHook.Signature (SignatureBody)
import Paddle.WebHook.SubscriptionCreated (SubscriptionCreated)
import Paddle.WebHook.SubscriptionCancelled (SubscriptionCancelled)
import Paddle.FieldModifier (modifier)

data PaddleWebHook passthrough 
  = SubscriptionCreatedWebHook (SubscriptionCreated passthrough)
  | SubscriptionCancelledWebHook (SubscriptionCancelled passthrough)
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
          { fieldLabelModifier = modifier
          }

      toWebHook :: Text -> Either Text (PaddleWebHook passthrough)
      toWebHook "subscription_created" = SubscriptionCreatedWebHook <$> genericFromForm formOptions form
      toWebHook "subscription_cancelled" = SubscriptionCancelledWebHook <$> genericFromForm formOptions form
      toWebHook name = Right $ UnknownWebHook name

type API passthrough
    = ReqBody '[FormUrlEncoded] (PaddleWebHook passthrough, SignatureBody)
    :> Post '[JSON] NoContent

api :: Proxy (API passthrough)
api = Proxy
