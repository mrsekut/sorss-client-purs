module Page.Register where

import Prelude

import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP



type Article =
  { title :: String
  , description :: String
  }


data Action
  = Initialize


type State =
  { articles :: List Article
  , page :: Int
  }


component :: forall query input output m. H.Component query input output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }
  where
  initialState :: ∀ i. i -> State
  initialState _ =
    { articles: Nil
    , page: 1
    }

  handleAction :: ∀ o slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> do
      pure unit

  render :: ∀ slots. State -> H.ComponentHTML Action slots m
  render _ =
    HH.div_
      [ HH.p_
        [ HH.button [HP.type_ HP.ButtonSubmit] [ HH.text "Next" ]
        , HH.text "register" ]
      ]