module Wechaty.Room
  ( Room
  , RoomT
  , runRoomT
  , sync
  , say
  , add
  , del
  , quit
  , topic
  , setTopic
  , announce
  , setAnnounce
  , alias
  , has
  , memberAll
  , member
  , memberList
  , owner
  ) where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Control.Monad.Reader (ask, ReaderT, runReaderT)
import Data.Maybe (Maybe)
import Wechaty.Contact (Contact)
import Wechaty.Utils (callp, call1p, toMaybe)

foreign import data Room :: Type
type RoomT m = ReaderT Room m

runRoomT :: forall a m. Room -> RoomT m a -> m a
runRoomT room = flip runReaderT room

sync :: forall m. MonadAff m => RoomT m Unit
sync = callp ask "sync"

say :: forall m. MonadAff m => String -> RoomT m Unit
say = call1p ask "say"

add :: forall m. MonadAff m => Contact -> RoomT m Unit
add = call1p ask "add"

del :: forall m. MonadAff m => Contact -> RoomT m Unit
del = call1p ask "del"

quit :: forall m. MonadAff m => RoomT m Unit
quit = callp ask "quit"

topic :: forall m. MonadAff m => RoomT m String
topic = callp ask "topic"

setTopic :: forall m. MonadAff m => String -> RoomT m Unit
setTopic = call1p ask "topic"

announce :: forall m. MonadAff m => RoomT m String
announce = callp ask "announce"

setAnnounce :: forall m. MonadAff m => String -> RoomT m Unit
setAnnounce = call1p ask "announce"

alias :: forall m. MonadAff m => Contact -> RoomT m String
alias = call1p ask "alias"

has :: forall m. MonadAff m => Contact -> RoomT m Boolean
has = call1p ask "has"

memberAll :: forall m. MonadAff m => String -> RoomT m (Array Contact)
memberAll = call1p ask "memberAll"

member :: forall m. MonadAff m => String -> RoomT m (Maybe Contact)
member q = toMaybe <$> call1p ask "member" q

memberList :: forall m. MonadAff m => RoomT m (Array Contact)
memberList = callp ask "memberList"

owner :: forall m. MonadAff m => RoomT m (Maybe Contact)
owner = toMaybe <$> callp ask "owner"
