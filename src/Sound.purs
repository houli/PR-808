module Sound
  ( Sound(..)
  , allSounds
  , playSound
  ) where

import Audio.Howler as Howl
import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Unit (Unit)

data Sound = Clap
           | Clave
           | Cowbell
           | Crash
           | HiHatClosed
           | HiHatOpen
           | HiTom
           | Kick
           | LowTom
           | MidTom
           | Rim
           | Shake
           | Snare

derive instance genericSound :: Generic Sound _
instance showSound :: Show Sound where
  show = genericShow

allSounds :: Array Sound
allSounds = [Clap, Clave, Cowbell, Crash, HiHatClosed, HiHatOpen, HiTom, Kick, LowTom, MidTom, Rim, Shake, Snare]

fileName :: Sound -> String
fileName s = "audio/" <> show s <> ".wav"

playSound :: forall e. Sound -> Number -> Eff (howler :: Howl.HOWLER | e) Unit
playSound s volume = do
  let file = fileName s
  sound <- Howl.new (Howl.defaultProps { buffer = true, urls = [file], volume = volume })
  Howl.play sound
