module Wechaty.Friendship
  ( Friendship
  , accept
  , hello
  , contact
  , type_
  ) where

import Prelude
import Effect.Aff (Aff)
import Wechaty.Contact (Contact)
import Effect (Effect)
import Wechaty.Utils (call, callp)

foreign import data Friendship :: Type

accept :: Friendship -> Aff Unit
accept s = callp (pure s) "accept"

hello :: Friendship -> Effect String
hello s = call (pure s) "hello"

contact :: Friendship -> Effect Contact
contact s = call (pure s) "contact"

type_ :: Friendship -> Effect Int
type_ s = call (pure s) "type"
