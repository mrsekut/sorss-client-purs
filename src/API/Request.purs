module API.Request where

import Prelude

import API.Endpoint (Endpoint, endpointCodec)
import Affjax (Request)
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Routing.Duplex (print)

newtype BaseURL = BaseURL String


data RequestMethod
  = Get

type RequestOptions =
  { endpoint :: Endpoint
  , method :: RequestMethod
  }


defaultRequest :: BaseURL -> RequestOptions -> Request String
defaultRequest (BaseURL baseUrl) { endpoint, method } =
  { method: Left requestMethod
  , url: baseUrl <> print endpointCodec endpoint
  , headers: []
  , content: RB.json <$> body
  , username: Nothing
  , password: Nothing
  , timeout: Nothing
  , withCredentials: false
  , responseFormat: RF.string
  }
  where
  Tuple requestMethod body = case method of
    Get -> Tuple GET Nothing