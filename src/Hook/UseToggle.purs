module Hook.UseToggle  where

import Prelude

import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Halogen.Hooks (class HookNewtype, type (<>), Hook, HookM, UseState)
import Halogen.Hooks as Hooks

type UseToggle' = UseState Boolean <> Hooks.Pure

foreign import data UseToggle :: Hooks.HookType
instance HookNewtype UseToggle UseToggle'

useToggle :: ∀ m. Boolean -> Hook m UseToggle (Tuple Boolean (HookM _ Unit))
useToggle init = Hooks.wrap hook
  where
  hook :: Hook m UseToggle' _
  hook = Hooks.do
    flg /\ flgId <- Hooks.useState init

    Hooks.pure $ flg /\ Hooks.modify_ flgId not