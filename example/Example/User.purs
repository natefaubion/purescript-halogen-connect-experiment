module Example.User where

import Prelude

import Halogen as H
import Halogen.Component.Connect (class MonadStore, ConnectInput, ConnectQuery, connect)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type Store r =
  { userName :: String
  , userEmail :: String
  | r
  }

type Input r = ConnectInput r Unit

data Query r a
  = Receive (Input r) a

component
  :: forall r m a
   . MonadStore a (Store r) m
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
      [ HH.text state.userName
      , HH.text "<"
      , HH.text state.userEmail
      , HH.text ">"
      ]

  eval :: Query (Store r) ~> H.ComponentDSL (Store r) (Query (Store r)) Void m
  eval = case _ of
    Receive { store } next ->
      H.put store $> next
