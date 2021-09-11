module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import AppM (runAppM)
import Component.Router as Router
import Data.Route (route)
import Store (LogLevel(..), Store)
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)



main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  let
    logLevel = Dev

  let
    initialStore :: Store
    initialStore = { logLevel }

  rootComponent <- runAppM initialStore Router.component
  halogenIO <- runUI rootComponent unit body
  void $ liftEffect $ matchesWith ( parse route ) \mOld new ->
    when ( mOld /= Just new ) do
      log $ show new
      launchAff_ $ halogenIO.query $ H.mkTell $ Router.Navigate new
  pure unit