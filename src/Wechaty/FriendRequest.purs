module Wechaty.FriendRequest
  ( FriendRequest
  , FriendRequestType (..)
  , createSend
  , createConfirm
  , createReceive
  , send
  , accept
  , reject
  , type_
  ) where

import Prelude
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Wechaty.Contact (Contact)
import Effect (Effect)
import Control.Promise (Promise, toAff)

foreign import data FriendRequest :: Type

data FriendRequestType =
    Unknown
  | Send
  | Receive
  | Confirm

foreign import createSend :: Contact -> String -> FriendRequest
foreign import createConfirm :: Contact -> FriendRequest
foreign import createReceive :: Contact -> String -> String -> FriendRequest
foreign import _send :: FriendRequest -> Effect (Promise Unit)
foreign import _accept :: FriendRequest -> Effect (Promise Unit)
foreign import _reject :: FriendRequest -> Effect (Promise Unit)
foreign import hello :: FriendRequest -> String
foreign import contact :: FriendRequest -> Contact
foreign import _type
  :: FriendRequestType -- Unknown
  -> FriendRequestType -- Send
  -> FriendRequestType -- Receive
  -> FriendRequestType -- Confirm
  -> FriendRequest -> FriendRequestType

type_ :: FriendRequest -> FriendRequestType
type_ = _type Unknown Send Receive Confirm

send :: FriendRequest -> Aff Unit
send req = liftEffect (_send req) >>= toAff

accept :: FriendRequest -> Aff Unit
accept req = liftEffect (_accept req) >>= toAff

reject :: FriendRequest -> Aff Unit
reject req = liftEffect (_reject req) >>= toAff
