module Wechaty
  ( Wechaty
  , WechatyConfig
  , WechatyT
  , runWechatyT
  , initWechaty
  , onScan
  , showQrcode
  , onError
  , onLogin
  , onLogout
  , onMessage
  , onFriendship
  , start
  , stop
  , logout
  , logonoff
  , userSelf
  , findContact
  , findContactAll
  , findRoom
  , findRoomAll
  -- , findMessage
  -- , findMessageAll
  , say
  , addFriendship
  ) where

import Prelude

import Effect.Aff.Class (class MonadAff, liftAff)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)
import Control.Monad.Reader (ask, ReaderT, runReaderT)
import Control.Promise (Promise, toAff)
import Wechaty.Contact (Contact, ContactT, runContactT)
import Wechaty.Room (Room)
import Wechaty.Message (MessageT, Message, runMessageT)
import Wechaty.Friendship (Friendship)
import Data.Maybe (Maybe (..))
import Wechaty.Utils (call, callp, call1p) as U

foreign import data Wechaty :: Type

data WechatyConfig m = WechatyConfig (m Unit -> Effect Unit) Wechaty

type WechatyT m = ReaderT (WechatyConfig m) m

runWechatyT
  :: forall a m. (m Unit -> Effect Unit)
  -> Wechaty -> WechatyT m a -> m a
runWechatyT runEff bot = flip runReaderT (WechatyConfig runEff bot)

wechaty :: forall m. Monad m => WechatyT m Wechaty
wechaty = do
  (WechatyConfig _ bot) <- ask
  pure bot

foreign import initWechaty :: Effect Wechaty

foreign import _on :: Wechaty -> String -> Effect Unit -> Effect Unit
foreign import _on1 :: forall a. Wechaty -> String -> (a -> Effect Unit) -> Effect Unit
foreign import _on2 :: forall a b. Wechaty -> String -> (a -> b -> Effect Unit) -> Effect Unit
foreign import _on3 :: forall a b c. Wechaty -> String -> (a -> b -> c -> Effect Unit) -> Effect Unit

mkOn :: forall m f. MonadEffect m => (Wechaty -> String -> f -> Effect Unit) -> String -> f -> WechatyT m Unit
mkOn wrap event f = do
  (WechatyConfig _ bot) <- ask
  liftEffect $ wrap bot event f

on :: forall m. MonadEffect m => String -> Effect Unit -> WechatyT m Unit
on = mkOn _on

on1 :: forall m a. MonadEffect m => String -> (a -> Effect Unit) -> WechatyT m Unit
on1 = mkOn _on1

on2 :: forall m a b. MonadEffect m => String -> (a -> b -> Effect Unit) -> WechatyT m Unit
on2 = mkOn _on2

on3 :: forall m a b c. MonadEffect m => String -> (a -> b -> c -> Effect Unit) -> WechatyT m Unit
on3 = mkOn _on3

onScan
  :: forall m. MonadEffect m
  => (String -> Int -> Effect Unit) -> WechatyT m Unit
onScan = on2 "scan"

onError
  :: forall m. MonadEffect m
  => (Error -> Effect Unit) -> WechatyT m Unit
onError = on1 "error"

onLogout
  :: forall m. MonadEffect m
  => ContactT m Unit -> WechatyT m Unit
onLogout m = do
  (WechatyConfig runEff _) <- ask
  on1 "logout" $ doContact runEff m

onLogin
  :: forall m. MonadEffect m
  => ContactT m Unit -> WechatyT m Unit
onLogin m = do
  (WechatyConfig runEff _) <- ask
  on1 "login" $ doContact runEff m

onMessage
  :: forall m. MonadEffect m
  => MessageT m Unit -> WechatyT m Unit
onMessage m = do
  (WechatyConfig runEff _) <- ask
  on1 "message" $ doMessage runEff m

onFriendship :: forall m. MonadEffect m => (Friendship -> m Unit) -> WechatyT m Unit
onFriendship f = do
  (WechatyConfig runEff _) <- ask
  on1 "friendship" $ runEff <<< f


foreign import showQrcode :: String -> (Effect Unit)

doContact
  :: forall m. (m Unit -> Effect Unit)
  -> ContactT m Unit -> Contact -> Effect Unit
doContact runEff m contact = runEff $ runContactT contact m

doMessage
  :: forall m. (m Unit -> Effect Unit)
   -> MessageT m Unit -> Message -> Effect Unit
doMessage runEff m = runEff <<< flip runMessageT m

call :: forall m a. MonadAff m => String -> WechatyT m a
call = U.call wechaty

callp :: forall m. MonadAff m => String -> WechatyT m Unit
callp = U.callp wechaty

start :: forall m. MonadAff m => WechatyT m Unit
start = callp "start"

stop :: forall m. MonadAff m => WechatyT m Unit
stop = callp "stop"

logout :: forall m. MonadAff m => WechatyT m Unit
logout = callp "logout"

logonoff :: forall m. MonadAff m => WechatyT m Boolean
logonoff = call "logonoff"

userSelf :: forall m. MonadAff m => WechatyT m Contact
userSelf = call "userSelf"

foreign import _find
  :: forall a. Wechaty -> String
  -> String
  -> (a -> Maybe a) -> Maybe a
  -> Effect (Promise (Maybe a))

find :: forall m a. MonadAff m => String -> String -> WechatyT m (Maybe a)
find m n = do
  (WechatyConfig _ bot) <- ask
  liftAff $ liftEffect (_find bot m n Just Nothing) >>= toAff

foreign import _findAll
  :: forall a. Wechaty -> String
  -> String
  -> Effect (Promise (Array a))

findAll :: forall m a. MonadAff m => String -> String -> WechatyT m (Array a)
findAll m n = do
  (WechatyConfig _ bot) <- ask
  liftAff $ liftEffect (_findAll bot m n) >>= toAff

findContact :: forall m. MonadAff m => String -> WechatyT m (Maybe Contact)
findContact = find "Contact"

findContactAll :: forall m. MonadAff m => String -> WechatyT m (Array Contact)
findContactAll = findAll "Contact"

findRoom :: forall m. MonadAff m => String -> WechatyT m (Maybe Room)
findRoom = find "Room"

findRoomAll :: forall m. MonadAff m => String -> WechatyT m (Array Room)
findRoomAll = findAll "Room"

-- findMessage :: forall m. MonadAff m => String -> WechatyT m (Maybe Message)
-- findMessage = find "Message"
--
-- findMessageAll :: forall m. MonadAff m => String -> WechatyT m (Array Message)
-- findMessageAll = findAll "Message"

say :: forall m. MonadAff m => String -> WechatyT m Unit
say = U.call1p wechaty "say"

foreign import _addFriendship :: Wechaty -> Contact -> String -> Effect (Promise Unit)

addFriendship :: forall m. MonadAff m => Contact -> String -> WechatyT m Unit
addFriendship c h = do
  (WechatyConfig _ bot) <- ask
  liftAff $ liftEffect (_addFriendship bot c h) >>= toAff
