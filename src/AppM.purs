module AppM where

import Prelude

import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.Store.Monad (StoreT, runStoreT)
import Capability.Navigate (class Navigate)
import Data.Route (route)
import Store as Store
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Safe.Coerce (coerce)



newtype AppM a = AppM (StoreT Store.Action Store.Store Aff a)

derive newtype instance Functor AppM
derive newtype instance Apply AppM
derive newtype instance Applicative AppM
derive newtype instance Bind AppM
derive newtype instance Monad AppM
derive newtype instance MonadEffect AppM
derive newtype instance MonadAff AppM


runAppM :: ∀ q i o. Store.Store -> H.Component q i o AppM -> Aff (H.Component q i o Aff)
runAppM store = runStoreT store Store.reduce <<< coerce

instance Navigate AppM where
  navigate = liftEffect <<< setHash <<< print route

