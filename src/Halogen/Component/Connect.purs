module Halogen.Component.Connect
  ( class MonadStore
  , connectedSource
  , readStore
  , updateStore
  , ConnectInput
  , ConnectQuery
  , liftConnectQuery
  , connect
  ) where

import Prelude

import Data.Coyoneda (Coyoneda, liftCoyoneda, unCoyoneda)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Component as HC
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query.HalogenM as HQ
import Unsafe.Coerce (unsafeCoerce)

class Monad m <= MonadStore a s m | m -> s a where
  connectedSource :: forall f. (s -> f H.SubscribeStatus) -> H.EventSource f m
  readStore :: m s
  updateStore :: a -> m Unit

data ConnectQueryX s f i o a
  = Initialize a
  | Update s a
  | Proxy (Coyoneda f a)
  | Receive i a
  | Raise o a

foreign import data ConnectQuery :: (Type -> Type) -> (Type -> Type)

type State s i =
  { store :: Maybe s
  , input :: i
  }

type ConnectInput s i =
  { store :: s
  , input :: i
  }

liftConnectQuery :: forall f a. f a -> ConnectQuery f a
liftConnectQuery = unsafeCoerce <<< Proxy <<< liftCoyoneda

coerceConnect
  :: forall m a s f i o
   . MonadStore a s m
  => HC.Component HH.HTML (ConnectQueryX s f i o) i o m
  -> HC.Component HH.HTML (ConnectQuery f) i o m
coerceConnect = unsafeCoerce
  -- This is only safe because of modular abstraction. We can't observe or
  -- construct a ConnectQuery for anything besides `f`. It's safe to erase
  -- the `s` parameter because the evidence is tied to the underlying `m`
  -- due to the functional dependency.

connect
  :: forall m a s f i o
   . MonadStore a s m
  => HC.Component HH.HTML f (ConnectInput s i) o m
  -> HC.Component HH.HTML (ConnectQuery f) i o m
connect = HC.unComponent \comp ->
  let
    render :: State s i -> HC.ParentHTML (ConnectQueryX s f i o) f Unit m
    render = case _ of
      { input, store: Just store } ->
        HH.slot unit (HC.mkComponent comp) { input, store } (HE.input Raise)
      _ ->
        HH.text ""

    eval :: ConnectQueryX s f i o ~> H.HalogenM (State s i) (ConnectQueryX s f i o) f Unit o m
    eval = case _ of
      Initialize next -> do
        currentStore <- H.lift readStore
        H.modify _ { store = Just currentStore }
        H.subscribe $ connectedSource \store -> Update store H.Listening
        pure next
      Proxy q ->
        unCoyoneda (\k q' -> do
          result <- H.query unit q'
          case result of
            Nothing -> HQ.halt "[Halogen.Component.Connect] Inner query failure"
            Just r  -> pure (k r)
        ) q
      Update s next  -> H.modify _ { store = Just s } $> next
      Receive i next -> H.modify _ { input = i } $> next
      Raise o next   -> H.raise o $> next

  in
    coerceConnect $ HC.lifecycleParentComponent
      { initialState: { store: Nothing, input: _ }
      , receiver: HE.input Receive
      , initializer: Just (Initialize unit)
      , finalizer: Nothing
      , render
      , eval
      }
