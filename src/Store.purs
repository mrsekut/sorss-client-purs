module Store where

import Prelude



type Store =
  { logLevel :: LogLevel
  }



data LogLevel = Dev | Prod

derive instance Eq LogLevel
derive instance Ord LogLevel



data Action
  = LogoutUser


reduce :: Store -> Action -> Store
reduce store = case _ of
  LogoutUser -> store
