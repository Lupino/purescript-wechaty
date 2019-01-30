module Wechaty.Utils
  ( call
  , call1
  , callp
  , call1p
  , property
  ) where

import Prelude
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Control.Promise (toAff)

foreign import _call :: forall obj a. obj -> String -> Effect a
foreign import _call1 :: forall obj arg a. obj -> String -> arg -> Effect a

foreign import property :: forall obj a. String -> obj -> a

call :: forall m obj a. MonadEffect m => m obj -> String -> m a
call o n = do
  o' <- o
  liftEffect $ _call o' n

call1 :: forall m obj arg a. MonadEffect m => m obj -> String -> arg -> m a
call1 o n a = do
  o' <- o
  liftEffect $ _call1 o' n a

callp :: forall m obj a. MonadAff m => m obj -> String -> m a
callp o n = do
  f <- call o n
  liftAff $ toAff f

call1p :: forall m obj arg a. MonadAff m => m obj -> String -> arg -> m a
call1p o n a = do
  f <- call1 o n a
  liftAff $ toAff f
