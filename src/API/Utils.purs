module Api.Utils where

import Prelude

import API.Request (RequestOptions, defaultRequest)
import Affjax (request)
import Data.Bifunctor (rmap)
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen.Store.Monad (class MonadStore, getStore)
import Simple.JSON as Simple
import Store (Action, Store)


mkRequest :: ∀ m
   . MonadAff m
  => MonadStore Action Store m
  => RequestOptions
  -> m (Maybe String)
mkRequest opts = do
  { baseUrl } <- getStore
  response <- liftAff $ request $ defaultRequest baseUrl opts
  pure $ hush $ rmap _.body response


-- FIXME: move
type Json = String


-- FIXME: interface a->a
decode :: ∀ m a
   . MonadAff m
  => Simple.ReadForeign a
  => (a -> a)
  -> Maybe Json
  -> m (Maybe a)
decode _ Nothing = pure Nothing
decode f (Just json) = case Simple.readJSON json of
  Right (r :: a) -> pure $ Just $ f r
  Left _ -> pure Nothing
