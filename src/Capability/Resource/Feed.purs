module Capability.Resource.Feed where

import Prelude

import Data.RSS (RSS)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)



class Monad m <= ManageFeed m where
  getFeeds :: m (Maybe RSS)


instance ManageFeed m => ManageFeed (HalogenM state action slots msg m) where
  getFeeds = lift getFeeds
