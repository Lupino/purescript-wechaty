module Wechaty.Room
  ( Room
  , RoomT
  , runRoomT
  , find
  , say
  , sayTo
  , getRoomTopic
  , roomTopic
  , findAll
  , delete
  ) where

import Prelude

import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect (Effect)
import Effect.Class (liftEffect)
import Control.Monad.Reader (ask, ReaderT, runReaderT)
import Control.Promise (Promise, toAff)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Maybe (Maybe(..))
import Wechaty.Contact (Contact)

foreign import data Room :: Type
type RoomT m = ReaderT Room m

runRoomT :: forall a m. Room -> RoomT m a -> m a
runRoomT room = flip runReaderT room

foreign import _find :: String
                     -> (Room -> Maybe Room)
                     -> Maybe Room
                     -> Effect (Promise (Maybe Room))

find :: String -> Aff (Maybe Room)
find n = liftEffect (_find n Just Nothing) >>= toAff

foreign import _findAll :: String -> Effect (Promise (Array Room))

findAll :: String -> Aff (Array Room)
findAll n = liftEffect (_findAll n) >>= toAff

foreign import _say :: forall a. Fn2 Room a (Effect (Promise Unit))

runSay :: forall a. Room -> a -> Aff Unit
runSay room a = liftEffect (runFn2 _say room a) >>= toAff

say
  :: forall a m. MonadAff m
  => a -> RoomT m Unit
say a = do
  room <- ask
  liftAff $ runSay room a

foreign import _sayTo :: forall a. Fn3 Room Contact a (Effect (Promise Unit))

runSayTo :: forall a. Room -> Contact -> a -> Aff Unit
runSayTo room contact a = liftEffect (runFn3 _sayTo room contact a) >>= toAff

sayTo
  :: forall a m. MonadAff m
  => Contact -> a -> RoomT m Unit
sayTo contact a = do
  room <- ask
  liftAff $ runSayTo room contact a

roomTopic
  :: forall m. Monad m
  => RoomT m String
roomTopic = getRoomTopic <$> ask

foreign import getRoomTopic :: Room -> String

foreign import _delete :: Room -> Contact -> Effect (Promise Unit)

delete :: forall m. MonadAff m => Contact -> RoomT m Unit
delete c = do
  room <- ask
  liftAff $ liftEffect (_delete room c) >>= toAff
