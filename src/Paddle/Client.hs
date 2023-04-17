{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}
module Paddle.Client 
 ( API(..)
 , PaddleError(..)
 , PaddleResponse(..)
 , responseToEither
 , client
 , runClient
 ) where

import           Data.Aeson (FromJSON, parseJSON, (.:), withObject)
import           Protolude 
import           Prelude ()
import           Network.HTTP.Client ( Manager )
import           Servant.API
import           Servant.Client hiding (client)
import qualified Servant.Client
import           Paddle.Client.DeleteModifier (DeleteModifier)
import           Paddle.Client.ListModifier (ListModifier)
import           Paddle.Client.GeneratePayLink (GeneratePayLink)
import           Paddle.Client.GeneratePayLinkResponse (GeneratePayLinkResponse)
import           Paddle.Client.ListModifierResponse (ListModifierResponse)
import           Paddle.Client.CreateModifier (CreateModifier)
import           Paddle.Client.CreateModifierResponse (CreateModifierResponse)
import           Paddle.Client.ListUsers (ListUsers)
import           Paddle.Client.ListUsersResponse (ListUsersResponse)
import           Paddle.Client.ListPayment (ListPayment)
import           Paddle.Client.ListPaymentResponse (ListPaymentResponse)
import           Paddle.Client.SubscriptionUsersUpdate (SubscriptionUsersUpdate)
import           Paddle.Client.SubscriptionUsersUpdateResponse (SubscriptionUsersUpdateResponse)

data PaddleError = PaddleError
  { message :: Text
  , code :: Int 
  } deriving (Show, Generic, FromJSON)

instance Exception Paddle.Client.PaddleError

data PaddleResponse a = 
  ResponseError PaddleError | ResponseSuccess a
  deriving (Show)

responseToEither :: PaddleResponse a -> Either PaddleError a
responseToEither (ResponseError e) = Left e
responseToEither (ResponseSuccess a) = Right a

instance FromJSON a => FromJSON (PaddleResponse a) where
  parseJSON = withObject "PaddleResponse" $ \v -> do
    isSuccessful <- v .: "success"
    if isSuccessful
    then ResponseSuccess <$> v .: "response"
    else ResponseError <$> v .: "error"

data API route = API
    { modifiersList :: route :-
        "subscription" :>
        "modifiers" :>
        ReqBody '[JSON] ListModifier :>
        Post '[JSON] (PaddleResponse [ListModifierResponse])
    , modifiersCreate :: route :-
        "subscription" :>
        "modifiers" :>
        "create" :>
        ReqBody '[JSON] CreateModifier :>
        Post '[JSON] (PaddleResponse CreateModifierResponse)
    , modifiersDelete :: route :-
        "subscription" :>
        "modifiers" :>
        "delete" :>
        ReqBody '[JSON] DeleteModifier :>
        Post '[JSON] (PaddleResponse (Maybe ())) -- https://github.com/bos/aeson/issues/744
    , paymentsList :: route :-
        "subscription" :>
        "payments" :>
        ReqBody '[JSON] ListPayment :>
        Post '[JSON] (PaddleResponse [ListPaymentResponse])
    , productGeneratePayLink :: route :-
        "product" :>
        "generate_pay_link" :>
        ReqBody '[JSON] GeneratePayLink :>
        Post '[JSON] (PaddleResponse GeneratePayLinkResponse)
    , usersList :: route :-
        "subscription" :>
        "users" :>
        ReqBody '[JSON] ListUsers :>
        Post '[JSON] (PaddleResponse [ListUsersResponse])
    , subscriptionUsersUpdate :: route :-
        "subscription" :>
        "users" :>
        "update" :>
        ReqBody '[JSON] SubscriptionUsersUpdate :>
        Post '[JSON] (PaddleResponse SubscriptionUsersUpdateResponse)
    } deriving (Generic)

api :: Proxy (ToServantApi API)
api = genericApi (Proxy :: Proxy API)

client :: API (AsClientT ClientM)
client = fromServant $ Servant.Client.client api

runClient :: Manager -> ClientM a -> IO (Either ClientError a)
runClient httpmanager cmd = do
  (`runClientM` env) cmd
  where
    env :: ClientEnv
    env = Servant.Client.mkClientEnv httpmanager (BaseUrl Https "vendors.paddle.com" 443 "api/2.0")
