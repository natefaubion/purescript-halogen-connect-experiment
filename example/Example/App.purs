module Example.App where

import Prelude

import Data.Const (Const)
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Example.Form as Form
import Example.Reducer (Action, Store)
import Example.User as User
import Halogen as H
import Halogen.Component.ChildPath (cp1, cp2)
import Halogen.Component.Connect (class MonadStore, ConnectQuery)
import Halogen.HTML as HH

type ChildSlot = Either2 Unit Int
type ChildQuery = Coproduct2 (ConnectQuery (Form.Query Store)) (ConnectQuery (User.Query Store))

component
  :: forall m
   . MonadStore Action Store m
  => H.Component HH.HTML (Const Void) Unit Void m
component =
  H.parentComponent
    { initialState: id
    , receiver: const Nothing
    , render
    , eval
    }
  where
  render _ =
    HH.div_
      [ HH.slot' cp1 unit Form.component unit absurd
      , HH.slot' cp2 1 User.component unit absurd
      , HH.slot' cp2 2 User.component unit absurd
      , HH.slot' cp2 3 User.component unit absurd
      ]

  eval :: Const Void ~> H.ParentDSL Unit (Const Void) ChildQuery ChildSlot Void m
  eval = absurd <<< unwrap
