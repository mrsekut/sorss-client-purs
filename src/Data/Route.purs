module Data.Route (Route(..), routeCodec) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Routing.Duplex (RouteDuplex', root)
import Routing.Duplex.Generic as G
import Routing.Duplex.Generic.Syntax ((/))



-- Route

data Route
  = Home
  | Feed

derive instance Eq Route
derive instance Ord Route

derive instance Generic Route _

instance Show Route where
  show = genericShow



routeCodec :: RouteDuplex' Route
routeCodec = root $ G.sum
  { "Home": G.noArgs
  , "Feed": "feed" / G.noArgs
  }

