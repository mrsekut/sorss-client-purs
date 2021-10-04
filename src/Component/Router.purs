module Component.Router where

import Prelude

import Capability.Navigate (class Navigate, navigate)
import Capability.Resource.Feed (class ManageFeed)
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Route (Route(..), routeCodec)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore)
import Page.Feed as Feed
import Page.Home as Home
import Routing.Duplex as RD
import Routing.Hash (getHash)
import Store as Store
import Type.Prelude (Proxy(..))
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (toEvent, MouseEvent)

type State =
  { route :: Maybe Route
  }

data Query a = Navigate Route a

data Action
  = Initialize
  | GoTo Route MouseEvent

-- FIXME: move Util
type OpaqueSlot slot = ∀ query. H.Slot query Void slot

type ChildSlots =
  ( home :: Home.Slot Unit
  , feed :: OpaqueSlot Unit
  )

component ::
  ∀ m.
  MonadAff m =>
  MonadStore Store.Action Store.Store m =>
  Navigate m =>
  ManageFeed m =>
  H.Component Query Unit Void m
component =
  H.mkComponent
  { initialState: const { route: Nothing }
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , handleQuery = handleQuery
    , initialize = Just Initialize
    }
  }
  where
  render :: State -> H.ComponentHTML Action ChildSlots m
  render st = navbar $ case st.route of
    Nothing -> HH.h1_ [ HH.text "Oh no! That page wasn't found" ]
    Just route -> case route of
      Home -> HH.slot Home._home unit Home.component unit absurd
      Feed -> HH.slot_ (Proxy :: _ "feed") unit Feed.component unit


handleAction ::
  ∀ o m.
  MonadEffect m =>
  Navigate m =>
  Action ->
  H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  Initialize -> do
    initialRoute <- hush <<< (RD.parse routeCodec) <$> H.liftEffect getHash
    navigate $ fromMaybe Home initialRoute
  GoTo route e -> do
    liftEffect $ preventDefault $ toEvent e
    mRoute <- H.gets _.route
    when ( mRoute /= Just route ) $ navigate route


handleQuery :: ∀ a o m. Query a -> H.HalogenM State Action ChildSlots o m ( Maybe a )
handleQuery = case _ of
  Navigate route a -> do
    mRoute <- H.gets _.route
    when (mRoute /= Just route) $
      H.modify_ _ { route = Just route }
    pure $ Just a



navbar :: ∀ a . HH.HTML a Action -> HH.HTML a Action
navbar html =
  HH.div_
  [ HH.ul_
    [ HH.li_
      [ HH.a
        [ HP.href "#"
        , HE.onClick (GoTo Home)
        ]
        [ HH.text "home" ]
      ]
    , HH.li_
      [ HH.a
        [ HP.href "#/feed"
        , HE.onClick (GoTo Feed)
        ]
        [ HH.text "feed" ]
      ]
    ]
  ,  html
  ]