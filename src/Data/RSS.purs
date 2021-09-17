module Data.RSS where

import Data.List (List)



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
  -- , items       :: List Item
  }


type Item =
  { title       :: String
  , link        :: String
  , description :: String
  , pubDate     :: String
  }
