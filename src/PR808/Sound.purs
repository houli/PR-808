module PR808.Sound
  ( allSounds
  , playSound
  ) where

import Audio.Howler as Howl
import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Unit (Unit)

import PR808.Types (Sound(..))

allSounds :: Array Sound
allSounds = [Clap, Clave, Cowbell, Crash, HiHatClosed, HiHatOpen, HiTom, Kick, LowTom, MidTom, Rim, Shake, Snare]

fileName :: Sound -> String
fileName s = "audio/" <> show s <> ".mp3"

playSound :: forall eff. Sound -> Number -> Eff (howler :: Howl.HOWLER | eff) Unit
playSound s volume = do
  let file = fileName s
  sound <- Howl.new (Howl.defaultProps { buffer = true, urls = [file], volume = volume })
  Howl.play sound
