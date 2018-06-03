module PR808.Sound
  ( allSounds
  , playSound
  ) where

import Audio.Howl (AudioSource(..), defaultOptions, new, play)
import Control.Bind (bind)
import Data.Function (($))
import Data.NonEmpty (singleton)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Unit (Unit)
import Effect (Effect)

import PR808.Types (Sound(..))

-- TODO: Use a spritesheet for all sounds
allSounds :: Array Sound
allSounds = [Clap, Clave, Cowbell, Crash, HiHatClosed, HiHatOpen, HiTom, Kick, LowTom, MidTom, Rim, Shake, Snare]

fileName :: Sound -> String
fileName s = "audio/" <> show s <> ".mp3"

playSound :: Sound -> Number -> Effect Unit
playSound s volume = do
  let file = fileName s
  sound <- new (AudioSource $ singleton file) defaultOptions { volume = volume }
  play sound
