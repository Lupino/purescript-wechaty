module Wechaty.Message
  ( say
  , sayTo
  , content
  , from
  , self
  , room
  , handleContact
  , handleRoom
  , Message
  , MessageT
  , runMessageT
  ) where

import Prelude

import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Control.Monad.Reader (ask, ReaderT, runReaderT)
import Control.Monad.Trans.Class (lift)
import Control.Promise (Promise, toAff)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Maybe (Maybe(..))
import Wechaty.Contact (Contact, ContactT, runContactT)
import Wechaty.Room (Room, RoomT, runRoomT)

foreign import data Message :: Type
type MessageT m = ReaderT Message m

runMessageT :: forall a m. Message -> MessageT m a -> m a
runMessageT msg = flip runReaderT msg


foreign import _say :: forall a. Fn2 Message a (Effect (Promise Unit))
foreign import _sayTo :: forall a. Fn3 Message Contact a (Effect (Promise Unit))
foreign import _getContent :: Message -> Effect String
foreign import _getFrom :: Message -> Effect Contact
foreign import _getSelf :: Message -> Effect Boolean
foreign import _room :: Fn3 (Room -> Maybe Room) (Maybe Room) Message (Effect (Maybe Room))

runSay :: forall a. Message -> a -> Aff Unit
runSay msg a = liftEffect (runFn2 _say msg a) >>= toAff

say
  :: forall a m. MonadAff m
  => a -> MessageT m Unit
say a = do
  msg <- ask
  liftAff $ runSay msg a

runSayTo :: forall a. Message -> Contact -> a -> Aff Unit
runSayTo msg contact a = liftEffect (runFn3 _sayTo msg contact a) >>= toAff

sayTo
  :: forall a m. MonadAff m
  => Contact -> a -> MessageT m Unit
sayTo contact a = do
  msg <- ask
  liftAff $ runSayTo msg contact a

content
  :: forall m. MonadEffect m
  => MessageT m String
content = do
  msg <- ask
  liftEffect $ _getContent msg

from
  :: forall m. MonadEffect m
  => MessageT m Contact
from = do
  msg <- ask
  liftEffect $ _getFrom msg

self
  :: forall m. MonadEffect m
  => MessageT m Boolean
self = do
  msg <- ask
  liftEffect $ _getSelf msg

room
  :: forall m. MonadEffect m
  => MessageT m (Maybe Room)
room = do
  msg <- ask
  liftEffect $ runFn3 _room Just Nothing msg

handleRoom
  :: forall m. MonadAff m
  => Room -> Boolean
  -> (Contact -> Boolean -> String -> RoomT m Unit)
  -> MessageT m Unit
handleRoom r manager m = do
  msg <- content
  f <- from
  lift $ runRoomT r (m f manager msg)

handleContact
  :: forall m. MonadEffect m
  => (String -> ContactT m Unit) -> MessageT m Unit
handleContact m = do
  msg <- content
  f <- from
  lift $ runContactT f (m msg)
