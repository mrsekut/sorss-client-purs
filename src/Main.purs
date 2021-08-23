module Main where

import Prelude

import Data.Const (Const)
import Data.List (List(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Page.RSSList as RSSList
import Type.Proxy (Proxy(..))



main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI rootComponent unit body


type Slots = (list :: forall query. H.Slot query Void Int)

_list = Proxy :: Proxy "list"

rootComponent :: H.Component (Const Void) Unit Void Aff
rootComponent =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval
    }
  where

  render :: ∀ m s a. s -> H.ComponentHTML a Slots m
  render _ =
    HH.div_
      [ HH.slot_ _list 0 RSSList.component { articles: Nil, page: 1} ]