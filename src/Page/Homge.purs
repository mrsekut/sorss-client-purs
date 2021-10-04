module Page.Home  where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Data.Tuple.Nested ((/\))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.Hooks as Hooks
import Hook.UseToggle (useToggle)
import Type.Proxy (Proxy(..))



type Slot p = ∀ query. H.Slot query Void p

_home :: Proxy "home"
_home = Proxy

component :: ∀ q i o m. MonadAff m => H.Component q i o m
component = Hooks.component \_ _ -> Hooks.do
  flg1 /\ setFlg1 <- useToggle false
  flg2 /\ setFlg2 <- useToggle false
  flg3 /\ setFlg3 <- useToggle false

  let update = if flg3 then setFlg1 else setFlg2

  Hooks.pure do
    HH.div_ [
       HH.div
         [ onClick \_ -> update]
         [ HH.text $ show flg1 <> show flg2],
       HH.div
         [ onClick \_ -> setFlg3]
         [ HH.text $ show flg3]
     ]