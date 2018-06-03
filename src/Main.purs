module Main where

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Coroutine as CR
import Control.Parallel (parTraverse_)
import Data.Function (flip, ($), (<<<))
import Data.Maybe (Maybe(..))
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Aff (Aff, forkAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import PR808.Component.DrumMachine as DM
import PR808.Component.PresetPicker as PP
import PR808.Presets (fifteenStep)
import PR808.Sound (allSounds, playSound)

-- TODO: Figure out how to use a sprite sheet rather than separate files
-- This is alright for now because these parallel requests are forked off
-- and won't block the UI from rendering
preloadSounds :: Aff Unit
preloadSounds = parTraverse_ (liftEffect <<< flip playSound 0.0) allSounds

main :: Effect Unit
main = do
  HA.runHalogenAff do
    _ <- forkAff preloadSounds
    body <- HA.awaitBody
    presetPicker <- runUI PP.presetPicker unit body
    machine <- runUI DM.drumMachine unit body
    presetPicker.subscribe $ CR.consumer \(PP.NotifyPresetChanged preset) -> do
      machine.query $ H.action $ DM.PresetSetup preset
      pure Nothing
    machine.query $ H.action $ DM.PresetSetup fifteenStep
