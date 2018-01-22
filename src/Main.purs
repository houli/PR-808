module Main where

import Audio.Howler (HOWLER)
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Coroutine as CR
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Timer (TIMER)
import Data.Foldable (for_)
import Data.Function (($))
import Data.Maybe (Maybe(..))
import Data.Unit (Unit, unit)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import PR808.Component.DrumMachine as D
import PR808.Component.PresetPicker as P
import PR808.Presets (fifteenStep)
import PR808.Sound (allSounds, playSound)

-- TODO: Figure out how to use a sprite sheet rather than separate files or run in Aff
-- possibly resolved in the next point release of Halogen
preloadSounds :: forall eff. Eff (howler :: HOWLER | eff) Unit
preloadSounds = for_ allSounds \sound -> do
  playSound sound 0.0

main :: Eff (HA.HalogenEffects (howler :: HOWLER, timer :: TIMER)) Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    presetPicker <- runUI P.presetPicker unit body
    machine <- runUI D.drumMachine unit body
    presetPicker.subscribe $ CR.consumer \(P.NotifyPresetChanged preset) -> do
      machine.query $ H.action $ D.PresetSetup preset
      pure Nothing
    machine.query $ H.action $ D.PresetSetup fifteenStep
  preloadSounds
