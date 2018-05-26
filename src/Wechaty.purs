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
  , start
  ) where

import Prelude

import Effect.Aff.Class (class MonadAff, liftAff)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Control.Monad.Reader (ask, ReaderT, runReaderT)
import Control.Promise (Promise, toAff)
import Data.Function.Uncurried (Fn2, runFn2)
import Wechaty.Contact (Contact, ContactT, runContactT)
import Wechaty.Message (MessageT, Message, runMessageT)

foreign import data Wechaty :: Type

data WechatyConfig m = WechatyConfig (m Unit -> Effect Unit) Wechaty

type WechatyT m = ReaderT (WechatyConfig m) m

runWechatyT
  :: forall a m. (m Unit -> Effect Unit)
  -> Wechaty -> WechatyT m a -> m a
runWechatyT runEff wechaty = flip runReaderT (WechatyConfig runEff wechaty)

foreign import initWechaty :: Effect Wechaty

foreign import _onScan :: Wechaty -> (String -> Int -> Effect Unit) -> (Effect Unit)

onScan
  :: forall m. MonadEffect m
  => (String -> Int -> Effect Unit) -> WechatyT m Unit
onScan f = do
  (WechatyConfig _ bot) <- ask
  liftEffect $ _onScan bot f

foreign import showQrcode :: String -> (Effect Unit)

foreign import _onError :: Wechaty -> (String -> Effect Unit) -> (Effect Unit)

onError
  :: forall m. MonadEffect m
  => (String -> Effect Unit) -> WechatyT m Unit
onError f = do
  (WechatyConfig _ bot) <- ask
  liftEffect $ _onError bot f

foreign import _onLogout :: Fn2 Wechaty (Contact -> Effect Unit) (Effect Unit)

onLogout
  :: forall m. MonadEffect m
  => ContactT m Unit -> WechatyT m Unit
onLogout m = do
  (WechatyConfig runEff bot) <- ask
  liftEffect $ runFn2 _onLogout bot $ doContact runEff m

foreign import _onLogin :: Fn2 Wechaty (Contact -> Effect Unit) (Effect Unit)

doContact
  :: forall m. (m Unit -> Effect Unit)
  -> ContactT m Unit -> Contact -> Effect Unit
doContact runEff m contact = runEff $ runContactT contact m

onLogin
  :: forall m. MonadEffect m
  => ContactT m Unit -> WechatyT m Unit
onLogin m = do
  (WechatyConfig runEff bot) <- ask
  liftEffect $ runFn2 _onLogin bot $ doContact runEff m

foreign import _onMessage :: Fn2 Wechaty (Message -> Effect Unit) (Effect Unit)

doMessage
  :: forall m. (m Unit -> Effect Unit)
   -> MessageT m Unit -> Message -> Effect Unit
doMessage runEff m = runEff <<< flip runMessageT m

onMessage
  :: forall m. MonadEffect m
  => MessageT m Unit -> WechatyT m Unit
onMessage m = do
  (WechatyConfig runEff bot) <- ask
  liftEffect $ runFn2 _onMessage bot $ doMessage runEff m

foreign import _start :: Wechaty -> Effect (Promise Unit)

start :: forall m. MonadAff m => WechatyT m Unit
start = do
  (WechatyConfig _ bot) <- ask
  liftAff $ liftEffect (_start bot) >>= toAff
