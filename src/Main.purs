module Main where

import Audio.Howl (HOWL)
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Coroutine as CR
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Timer (TIMER)
import Data.Function (($))
import Data.Maybe (Maybe(..))
import Data.Unit (Unit, unit)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import PR808.Component.DrumMachine as DM
import PR808.Component.PresetPicker as PP
import PR808.Presets (fifteenStep)

main :: Eff (HA.HalogenEffects (howl :: HOWL, timer :: TIMER)) Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    presetPicker <- runUI PP.presetPicker unit body
    machine <- runUI DM.drumMachine unit body
    presetPicker.subscribe $ CR.consumer \(PP.NotifyPresetChanged preset) -> do
      machine.query $ H.action $ DM.PresetSetup preset
      pure Nothing
    machine.query $ H.action $ DM.PresetSetup fifteenStep
