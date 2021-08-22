module Main where

import Prelude

import Control.Monad.State (get, put)
import Data.Const (Const)

import Effect (Effect)
import Effect.Aff (Aff)

import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)



-- | Store

store :: H.Component (Const Void) Unit Void Aff
store =
  H.mkComponent
    { initialState: const false
    , render: toggleButton
    , eval: H.mkEval $ H.defaultEval { handleAction = reducer }
    }



-- | Modules

data Action = Toggle
type State = Boolean

reducer :: Action -> H.HalogenM State Action () Void Aff Unit
reducer = case _ of
  Toggle -> do
    old <- get
    put $ not old



-- | Components

toggleButton :: State -> H.ComponentHTML Action () Aff
toggleButton isOn =
    HH.button
      [ HE.onClick \_ -> Toggle ]
      [ HH.text $ "The button is " <> if isOn then "ON" else "OFF" ]



-- | main

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    runUI store unit body