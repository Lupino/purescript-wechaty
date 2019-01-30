module Wechaty.Message
  ( handleContact
  , handleRoom
  , Message
  , MessageT
  , runMessageT
  , from
  , to
  , room
  , text
  , say
  , messageType
  , self
  , mention
  , mentionSelf
  , forward
  , toContact
  ) where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Control.Monad.Reader (ask, ReaderT, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe)
import Wechaty.Contact (Contact, ContactT, runContactT)
import Wechaty.Room (Room, RoomT, runRoomT)
import Wechaty.Utils (call, callp, call1p, toMaybe)

foreign import data Message :: Type
type MessageT m = ReaderT Message m

runMessageT :: forall a m. Message -> MessageT m a -> m a
runMessageT msg = flip runReaderT msg

from :: forall m. MonadEffect m => MessageT m Contact
from = call ask "from"

to :: forall m. MonadEffect m => MessageT m Contact
to = call ask "to"

room :: forall m. MonadEffect m => MessageT m (Maybe Room)
room = toMaybe <$> call ask "root"

text :: forall m. MonadEffect m => MessageT m String
text = call ask "text"

say :: forall m. MonadAff m => String -> MessageT m Unit
say = call1p ask "say"

messageType :: forall m. MonadEffect m => MessageT m String
messageType = call ask "type"

self :: forall m. MonadEffect m => MessageT m Boolean
self = call ask "self"

mention :: forall m. MonadEffect m => MessageT m (Array Contact)
mention = call ask "mention"

mentionSelf :: forall m. MonadEffect m => MessageT m Boolean
mentionSelf = call ask "mentionSelf"

forward :: forall m. MonadAff m => Contact -> MessageT m Unit
forward = call1p ask "forward"

age :: forall m. MonadEffect m => MessageT m Int
age = call ask "age"

toContact :: forall m. MonadAff m => MessageT m Contact
toContact = callp ask "toContact"

handleRoom
  :: forall m. MonadAff m
  => Room -> Boolean
  -> (Contact -> Boolean -> String -> RoomT m Unit)
  -> MessageT m Unit
handleRoom r manager m = do
  msg <- text
  f <- from
  lift $ runRoomT r (m f manager msg)

handleContact
  :: forall m. MonadEffect m
  => (String -> ContactT m Unit) -> MessageT m Unit
handleContact m = do
  msg <- text
  f <- from
  lift $ runContactT f (m msg)
