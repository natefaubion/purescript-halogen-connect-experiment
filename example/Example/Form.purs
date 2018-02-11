module Example.Form where

import Prelude

import Example.Reducer (Action(..))
import Halogen as H
import Halogen.Component.Connect (class MonadStore, ConnectInput, ConnectQuery, connect, updateStore)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Store r =
  { userName :: String
  , userEmail :: String
  | r
  }

type Input r = ConnectInput r Unit

data Query r a
  = Receive (Input r) a
  | Store Action a

component
  :: forall r m
   . MonadStore Action (Store r) m
  => H.Component HH.HTML (ConnectQuery (Query (Store r))) Unit Void m
component =
  connect $ H.component
    { initialState: _.store
    , receiver: HE.input Receive
    , render
    , eval
    }
  where
  render :: Store r -> H.ComponentHTML (Query (Store r))
  render state =
    HH.div_
      [ HH.input
          [ HP.value state.userName
          , HP.placeholder "User name"
          , HE.onValueInput (HE.input (Store <<< UpdateUserName))
          ]
      , HH.input
          [ HP.value state.userEmail
          , HP.placeholder "User email"
          , HE.onValueInput (HE.input (Store <<< UpdateUserEmail))
          ]
      ]

  eval :: Query (Store r) ~> H.ComponentDSL (Store r) (Query (Store r)) Void m
  eval = case _ of
    Receive { store } next ->
      H.put store $> next
    Store action next ->
      H.lift (updateStore action) $> next

