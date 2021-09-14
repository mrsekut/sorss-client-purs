module API.Endpoint where

import Prelude

import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', prefix, root)
import Routing.Duplex.Generic as G
import Routing.Duplex.Generic.Syntax ((/))

data Endpoint
  = RSS

derive instance Generic Endpoint _


endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ prefix "api" $ G.sum
  { "RSS": "rss" / G.noArgs
  }
