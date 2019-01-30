module Wechaty.Contact
  ( Contact
  , ContactT
  , runContactT
  , say
  , getContactName
  , contactName
  , self
  ) where

import Prelude

import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect (Effect)
import Effect.Class (liftEffect)
import Control.Monad.Reader (ask, ReaderT, runReaderT)
import Control.Promise (Promise, toAff)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..))

foreign import data Contact :: Type
type ContactT m = ReaderT Contact m

runContactT :: forall a m. Contact -> ContactT m a -> m a
runContactT contact = flip runReaderT contact

foreign import _say :: forall a. Fn2 Contact a (Effect (Promise Unit))

runSay :: forall a. Contact -> a -> Aff Unit
runSay contact a = liftEffect (runFn2 _say contact a) >>= toAff

say
  :: forall a m. MonadAff m
  => a -> ContactT m Unit
say a = do
  contact <- ask
  liftAff $ runSay contact a

contactName
  :: forall m. Monad m
  => ContactT m String
contactName = getContactName <$> ask

foreign import getContactName :: Contact -> String

foreign import self :: Effect Contact
