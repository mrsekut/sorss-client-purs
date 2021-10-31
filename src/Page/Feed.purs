module Page.Feed where

import Prelude

import Capability.Navigate (class Navigate)
import Capability.Resource.Feed (class ManageFeed, getFeeds)
import Data.Maybe (Maybe(..))
import Data.RSS (RSS, Item)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore)
import Network.RemoteData (RemoteData(..), fromMaybe)
import Store as Store
import Type.Prelude (Proxy(..))


data Action
  = Initialize
  | LoadFeeds


type State =
  { feeds :: RemoteData String RSS
  }

type Slot p = ∀ query. H.Slot query Void p


_feed :: Proxy "feed"
_feed = Proxy


component ::
  ∀ q o m.
  MonadAff m =>
  MonadStore Store.Action Store.Store m =>
  Navigate m =>
  ManageFeed m =>
  H.Component q Unit o m
component =
  H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }
  where
  initialState _ =
    { feeds: NotAsked
    }

  render :: ∀ slots. State -> H.ComponentHTML Action slots m
  render { feeds } =
    HH.div_
      [ HH.h1_ [ HH.text "Feed Page" ]
      , HH.div_ [ renderTitle feeds ]
      , HH.button
          [ HE.onClick \_ -> LoadFeeds ]
          [ HH.text $ " load "]
      ]
    where
    renderTitle :: RemoteData String RSS -> H.ComponentHTML Action slots m
    renderTitle = case _ of
      NotAsked ->
        HH.div_
          [ HH.text "RSS not loaded" ]
      Loading ->
        HH.div_
          [ HH.text "Loading RSS" ]
      Failure err ->
        HH.div_
          [ HH.text $ "Failed loading RSS: " <> err ]
      Success { sChannels } ->
        HH.div_
          [ HH.text sChannels.title
          , HH.ul_ $ map listItem sChannels.items
          ]

  handleAction :: ∀ slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> do
      void $ H.fork $ handleAction LoadFeeds
    LoadFeeds -> do
      H.modify_ _ { feeds = Loading }
      feeds <- getFeeds
      log $ show feeds
      H.modify_ _ { feeds = fromMaybe feeds }


listItem :: ∀ slots m. Item -> H.ComponentHTML Action slots m
listItem item =
  HH.div_
    [ HH.div_ [HH.text item.title]
    , HH.div_
      [ HH.a [ HP.href item.link] [ HH.text "link" ] ]
    , HH.div_
      [ HH.text $ case item.description of
          Nothing -> ""
          Just desc -> desc
      ]
    ]
