module Data.RSS where

import Data.Maybe (Maybe)



type RSS =
  { sVersion  :: Number
  , sChannels :: Channels
  }


type Channels =
  { title       :: String
  , link        :: String
  , description :: String
  , language    :: String
  , copyright   :: String
  , pubDate     :: String
  , items       :: Array Item
  }


type Item =
  { itemTitle       :: String
  , itemLink        :: String
  , itemDescription :: Maybe String
  , itemPubDate     :: String
  }
