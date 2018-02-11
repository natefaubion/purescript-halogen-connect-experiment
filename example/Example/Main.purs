module Example.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Example.App (component)
import Example.Monad (runStore)
import Example.Reducer (initialStore, reduce)
import FRP (FRP)
import Halogen.Aff (HalogenEffects, awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)

main :: Eff (HalogenEffects (frp :: FRP)) Unit
main = runHalogenAff do
  ui <- runStore initialStore reduce component
  body <- awaitBody
  runUI ui unit body
