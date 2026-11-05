{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Paddle.Billing.Client
  ( API(..)
  , ApiKey(..)
  , Environment(..)
  , BillingClientError(..)
  , client
  , baseUrl
  , runClient
  , classifyClientError
  ) where

import Data.Aeson (eitherDecode)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager)
import Paddle.Billing.Address
import Paddle.Billing.Catalog
import Paddle.Billing.Customer
import Paddle.Billing.Portal
import Paddle.Billing.Settings
import Paddle.Billing.Subscription
import Paddle.Billing.Transaction
import Paddle.Billing.Types hiding (Patch)
import Servant.API
import Servant.Client hiding (client, Response, baseUrl)
import qualified Servant.Client as Servant

-- | Secret API token. Its Show instance deliberately redacts the token.
newtype ApiKey = ApiKey Text

instance Show ApiKey where
  show _ = "ApiKey <redacted>"

data Environment = Sandbox | Live
  deriving (Eq, Show)

-- | Retain the original HTTP error, including status and headers, alongside
-- Paddle's structured error. Non-JSON and network failures remain usable.
data BillingClientError
  = BillingApiError ErrorResponse ClientError
  | BillingTransportError ClientError
  deriving (Show)

data API route = API
  { checkoutUrlGet :: route :- "settings" :> "checkout-url"
      :> Get '[JSON] (Response CheckoutUrlSettings)
  , checkoutUrlUpdate :: route :- "settings" :> "checkout-url"
      :> ReqBody '[JSON] CheckoutUrlSettings :> Patch '[JSON] (Response CheckoutUrlSettings)
  , productsCreate :: route :- "products"
      :> ReqBody '[JSON] CreateProduct :> PostCreated '[JSON] (Response Product)
  , productsGet :: route :- "products" :> Capture "product_id" ProductId
      :> Get '[JSON] (Response Product)
  , productsArchive :: route :- "products" :> Capture "product_id" ProductId
      :> ReqBody '[JSON] Archive :> Patch '[JSON] (Response Product)
  , pricesCreate :: route :- "prices"
      :> ReqBody '[JSON] CreatePrice :> PostCreated '[JSON] (Response Price)
  , pricesGet :: route :- "prices" :> Capture "price_id" PriceId
      :> Get '[JSON] (Response Price)
  , pricesArchive :: route :- "prices" :> Capture "price_id" PriceId
      :> ReqBody '[JSON] Archive :> Patch '[JSON] (Response Price)
  , transactionsCreate :: route :- "transactions"
      :> ReqBody '[JSON] CreateTransaction :> PostCreated '[JSON] (Response Transaction)
  , transactionsGet :: route :- "transactions" :> Capture "transaction_id" TransactionId
      :> Get '[JSON] (Response Transaction)
  , transactionsUpdate :: route :- "transactions" :> Capture "transaction_id" TransactionId
      :> ReqBody '[JSON] UpdateTransaction :> Patch '[JSON] (Response Transaction)
  , customersCreate :: route :- "customers"
      :> ReqBody '[JSON] CreateCustomer :> PostCreated '[JSON] (Response Customer)
  , customersGet :: route :- "customers" :> Capture "customer_id" CustomerId
      :> Get '[JSON] (Response Customer)
  , customersUpdate :: route :- "customers" :> Capture "customer_id" CustomerId
      :> ReqBody '[JSON] UpdateCustomer :> Patch '[JSON] (Response Customer)
  , addressesCreate :: route :- "customers" :> Capture "customer_id" CustomerId
      :> "addresses" :> ReqBody '[JSON] CreateAddress :> PostCreated '[JSON] (Response Address)
  , addressesList :: route :- "customers" :> Capture "customer_id" CustomerId
      :> "addresses" :> QueryParam "after" AddressId :> QueryParam "per_page" Int
      :> Get '[JSON] (Response [Address])
  , addressesGet :: route :- "customers" :> Capture "customer_id" CustomerId
      :> "addresses" :> Capture "address_id" AddressId :> Get '[JSON] (Response Address)
  , addressesUpdate :: route :- "customers" :> Capture "customer_id" CustomerId
      :> "addresses" :> Capture "address_id" AddressId
      :> ReqBody '[JSON] UpdateAddress :> Patch '[JSON] (Response Address)
  , subscriptionsGet :: route :- "subscriptions" :> Capture "subscription_id" SubscriptionId
      :> Get '[JSON] (Response Subscription)
  , subscriptionsList :: route :- "subscriptions"
      :> QueryParam "customer_id" CustomerId :> QueryParam "after" SubscriptionId
      :> QueryParam "per_page" Int :> Get '[JSON] (Response [Subscription])
  , subscriptionsUpdate :: route :- "subscriptions" :> Capture "subscription_id" SubscriptionId
      :> ReqBody '[JSON] UpdateSubscription :> Patch '[JSON] (Response Subscription)
  , subscriptionsPreview :: route :- "subscriptions" :> Capture "subscription_id" SubscriptionId
      :> "preview" :> ReqBody '[JSON] UpdateSubscription
      :> Patch '[JSON] (Response SubscriptionPreview)
  , subscriptionsCancel :: route :- "subscriptions" :> Capture "subscription_id" SubscriptionId
      :> "cancel" :> ReqBody '[JSON] CancelSubscription :> Post '[JSON] (Response Subscription)
  , portalSessionsCreate :: route :- "customers" :> Capture "customer_id" CustomerId
      :> "portal-sessions" :> ReqBody '[JSON] CreatePortalSession
      :> PostCreated '[JSON] (Response PortalSession)
  } deriving (Generic)

type AuthenticatedAPI = Header' '[Required, Strict] "Authorization" Text
  :> Header' '[Required, Strict] "Paddle-Version" Text :> ToServantApi API

-- | All requests authenticate with a bearer token and pin API version 1.
client :: ApiKey -> API (AsClientT ClientM)
client (ApiKey token) = fromServant $
  Servant.client (Proxy :: Proxy AuthenticatedAPI) ("Bearer " <> token) "1"

baseUrl :: Environment -> BaseUrl
baseUrl Sandbox = BaseUrl Https "sandbox-api.paddle.com" 443 ""
baseUrl Live = BaseUrl Https "api.paddle.com" 443 ""

-- | The supplied manager must support TLS. No mutation is retried automatically.
runClient :: Manager -> Environment -> ClientM a -> IO (Either BillingClientError a)
runClient httpManager environment action = do
  result <- runClientM action (mkClientEnv httpManager (baseUrl environment))
  pure $ either (Left . classifyClientError) Right result

classifyClientError :: ClientError -> BillingClientError
classifyClientError err@(FailureResponse _ response) =
  case eitherDecode (responseBody response) of
    Right paddleError -> BillingApiError paddleError err
    Left _ -> BillingTransportError err
classifyClientError err = BillingTransportError err
