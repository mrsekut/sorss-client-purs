module Main where

import Prelude

import API.Request (BaseURL(..))
import AppM (runAppM)
import Component.Router as Router
import Data.Maybe (Maybe(..))
import Data.Route (routeCodec)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)
import Store (Store)



main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  let
    baseUrl = BaseURL "https://localhost:8081"

  let
    initialStore :: Store
    initialStore = { baseUrl }

  rootComponent <- runAppM initialStore Router.component
  halogenIO <- runUI rootComponent unit body
  void $ liftEffect $ matchesWith (parse routeCodec) \mOld new ->
    when (mOld /= Just new) do
      log $ show new
      launchAff_ $ halogenIO.query $ H.mkTell $ Router.Navigate new
  pure unit