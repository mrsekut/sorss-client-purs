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
import Halogen.Store.Monad (class MonadStore)
import Network.RemoteData (RemoteData(..), fromMaybe)
import Store as Store
import Type.Prelude (Proxy(..))
import Halogen.HTML.Events as HE


data Action
  = Initialize
  | LoadFeeds


type State =
  { feeds :: RemoteData String RSS
  }

type Slot p = ∀ query. H.Slot query Void p


_feed :: Proxy "feed"
_feed = Proxy


component :: ∀ q o m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => ManageFeed m
  => H.Component q Unit o m
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
    HH.div_ [
      HH.h1_ [
        HH.text "Feed Page"
      ]
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
      Success feeds ->
        HH.div_ [
          HH.text feeds.sChannels.title
          , HH.ul_ $ map renderItem feeds.sChannels.items
        ]

    renderItem ::Item -> H.ComponentHTML Action slots m
    renderItem item =
      HH.div_ [ HH.text item.itemTitle ]

  handleAction :: ∀ slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> do
      void $ H.fork $ handleAction LoadFeeds
    LoadFeeds -> do
      H.modify_ _ { feeds = Loading }
      feeds <- getFeeds
      log $ show feeds
      H.modify_ _ { feeds = fromMaybe feeds }