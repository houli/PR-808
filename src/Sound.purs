module Sound
  ( Sound(..)
  , allSounds
  , playSound
  ) where

import Prelude

import Audio.Howler as Howl
import Control.Monad.Eff (Eff)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

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

playSound :: forall eff. Sound -> Eff (howler :: Howl.HOWLER | eff) Unit
playSound s = do
  let file = fileName s
  sound <- Howl.new (Howl.defaultProps { buffer = true, urls = [file] })
  Howl.play sound
