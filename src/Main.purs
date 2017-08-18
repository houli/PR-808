module Main where

import Audio.Howler (HOWLER)
import Control.Bind (bind, discard)
import Control.Monad.Eff (Eff)
import Data.Foldable (for_)
import Data.Unit (Unit, unit)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Component.Track (track)
import Sound (allSounds, playSound)

-- TODO: Figure out how to use a sprite sheet rather than separate files or run in Aff
preloadSounds :: forall e. Eff (howler :: HOWLER | e) Unit
preloadSounds = for_ allSounds \sound -> do
  playSound sound 0.0

main :: Eff (HA.HalogenEffects (howler :: HOWLER)) Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI track unit body
  preloadSounds
