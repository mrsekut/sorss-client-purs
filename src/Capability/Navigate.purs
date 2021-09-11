module Capability.Navigate where

import Prelude

import Data.Route (Route)
import Halogen (HalogenM, lift)



class Monad m <= Navigate m where
  navigate :: Route -> m Unit


instance Navigate m => Navigate (HalogenM state action slots msg m) where
  navigate = lift <<< navigate
