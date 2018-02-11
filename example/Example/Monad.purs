module Example.Monad where

import Prelude

import Control.Coroutine as CC
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR, takeVar)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (makeEmptyVar, putVar)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef, writeRef)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Bifunctor (class Bifunctor)
import Data.Either (Either(..))
import FRP (FRP)
import FRP.Event (Event, create, subscribe)
import Halogen (hoist)
import Halogen as H
import Halogen.Component.Connect (class MonadStore)
import Halogen.Query.EventSource (EventSource(..))
import Type.Row.Effect.Equality (class EffectRowEquals, effFrom, effTo)

type Env action store eff =
  { event  :: Event store
  , push   :: store -> Eff eff Unit
  , update :: store -> action -> store
  , value  :: Ref store
  }

newtype StoreM action store eff a = StoreM (ReaderT (Env action store eff) (Aff eff) a)

derive newtype instance functorStoreM :: Functor (StoreM action store eff)
derive newtype instance applyStoreM :: Apply (StoreM action store eff)
derive newtype instance applicativeStoreM :: Applicative (StoreM action store eff)
derive newtype instance bindStoreM :: Bind (StoreM action store eff)
derive newtype instance monadStoreM :: Monad (StoreM action store eff)
derive newtype instance monadEffStoreM :: MonadEff eff (StoreM action store eff)
derive newtype instance monadAffStoreM :: MonadAff eff (StoreM action store eff)

type StoreEffects eff =
  ( ref :: REF
  , frp :: FRP
  , avar :: AVAR
  | eff
  )

instance monadConnectStoreM :: EffectRowEquals eff (StoreEffects eff2) => MonadStore action store (StoreM action store eff) where
  readStore = StoreM $ liftEff <<< effFrom <<< readRef <<< _.value =<< ask
  updateStore action = StoreM $ do
    env <- ask
    liftEff $ effFrom do
      store <- readRef env.value
      let newStore = env.update store action
      writeRef env.value newStore
      effTo $ env.push newStore
  connectedSource k = EventSource $ StoreM do
    env <- ask
    liftEff $ effFrom do
      var <- effFrom makeEmptyVar
      unsubscribe <- subscribe env.event \a -> putVar a var (const (pure unit))
      let
        producer = CC.producer $ Left <<< k <$> liftAff (effFrom (takeVar var))
        done = liftEff $ effFrom unsubscribe
      pure { producer, done }

runStore
  :: forall eff action store h q i o m
   . MonadAff (StoreEffects eff) m
  => Bifunctor h
  => store
  -> (store -> action -> store)
  -> H.Component h q i o (StoreM action store (StoreEffects eff))
  -> m (H.Component h q i o m)
runStore initialStore update comp = liftAff do
  env <- liftEff do
    { event, push } <- create
    value <- newRef initialStore
    pure { event, push, update, value }
  pure $ hoist (\(StoreM m) -> liftAff $ runReaderT m env) comp
