module Store where

import Prelude

import API.Request (BaseURL)


type Store =
  { baseUrl :: BaseURL
  }


data Action


reduce :: Store -> Action -> Store
reduce store _ = store