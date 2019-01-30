module Wechaty.Contact
  ( Contact
  , ContactT
  , runContactT
  , say
  , sync
  , self
  , name
  , alias
  , setAlias
  , friend
  , province
  , city
  , name'
  ) where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Control.Monad.Reader (ask, ReaderT, runReaderT)
import Wechaty.Utils (call, callp, call1, call1p, property)

foreign import data Contact :: Type
type ContactT m = ReaderT Contact m

runContactT :: forall a m. Contact -> ContactT m a -> m a
runContactT contact = flip runReaderT contact

say :: forall m. MonadAff m => String -> ContactT m Unit
say = call1p ask "say"

sync :: forall m. MonadAff m => ContactT m Unit
sync = callp ask "sync"

self :: forall m. MonadAff m => ContactT m Boolean
self = call ask "self"

name :: forall m. MonadAff m => ContactT m String
name = call ask "name"

alias :: forall m. MonadAff m => ContactT m String
alias = call ask "alias"

setAlias :: forall m. MonadAff m => String -> ContactT m String
setAlias = call1 ask "alias"

friend :: forall m. MonadAff m => ContactT m Boolean
friend = call ask "friend"

province :: forall m. MonadAff m => ContactT m String
province = call ask "province"

city :: forall m. MonadAff m => ContactT m String
city = call ask "city"

name' :: Contact -> String
name' = property "name"
