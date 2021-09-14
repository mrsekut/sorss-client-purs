module Data.RSS where

import Prelude

import Control.Alt ((<|>))
import Data.Generic.Rep as GR
import Data.List (List)
import Data.Show.Generic (genericShow)
import Foreign (Foreign)
import Foreign as Foreign
import Simple.JSON as Simple


-- data IntOrBoolean2
--   = Int2 Int
--   | Boolean2 Boolean


-- derive instance GR.Generic IntOrBoolean2 _

-- instance Show IntOrBoolean2 where
--   show = genericShow


-- instance Simple.ReadForeign IntOrBoolean2 where
--   readImpl f = GR.to <$> untaggedSumRep f


-- class UntaggedSumRep rep where
--   untaggedSumRep :: Foreign -> Foreign.F rep


-- instance
--   ( UntaggedSumRep a
--   , UntaggedSumRep b
--   ) => UntaggedSumRep (GR.Sum a b) where
--   untaggedSumRep f
--       = GR.Inl <$> untaggedSumRep f
--     <|> GR.Inr <$> untaggedSumRep f

-- instance
--   ( UntaggedSumRep a
--   ) => UntaggedSumRep (GR.Constructor name a) where
--   untaggedSumRep f = GR.Constructor <$> untaggedSumRep f

-- instance
--   ( Simple.ReadForeign a
--   ) => UntaggedSumRep (GR.Argument a) where
--   untaggedSumRep f = GR.Argument <$> Simple.readImpl f




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

-- derive instance GR.Generic RSS _
-- derive instance GR.Generic Channels _
-- derive instance GR.Generic Item _