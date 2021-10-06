module AppM where

import Prelude

import API.Endpoint as Endpoint
import API.Request (RequestMethod(..))
import Api.Utils (decode, mkRequest)
import Capability.Navigate (class Navigate)
import Capability.Resource.Feed (class ManageFeed)
import Data.RSS (RSS)
import Data.Route (routeCodec)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, runStoreT)
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Safe.Coerce (coerce)
import Store (Action, Store)
import Store as Store
import Type.Prelude (Proxy(..))

newtype AppM a = AppM (StoreT Store.Action Store.Store Aff a)

derive newtype instance Functor AppM
derive newtype instance Apply AppM
derive newtype instance Applicative AppM
derive newtype instance Bind AppM
derive newtype instance Monad AppM
derive newtype instance MonadEffect AppM
derive newtype instance MonadAff AppM
derive newtype instance MonadStore Action Store AppM


runAppM :: ∀ q i o. Store.Store -> H.Component q i o AppM -> Aff (H.Component q i o Aff)
runAppM store = runStoreT store Store.reduce <<< coerce


instance Navigate AppM where
  navigate = liftEffect <<< setHash <<< print routeCodec



instance ManageFeed AppM where
  getFeeds = do
    res <- mkRequest { endpoint: Endpoint.RSS, method: Get }
    decode (Proxy :: Proxy RSS) res