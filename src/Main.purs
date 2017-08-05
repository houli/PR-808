module Main where

import Prelude

import Audio.Howler (HOWLER)
import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Component (component)

main ∷ Eff (HA.HalogenEffects (howler ∷ HOWLER)) Unit
main = HA.runHalogenAff do
  body ← HA.awaitBody
  runUI component unit body
