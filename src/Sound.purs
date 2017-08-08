module Sound
  ( Sound(..)
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

fileName :: Sound -> String
fileName s = "audio/" <> show s <> ".wav"

playSound :: forall eff. Sound -> Eff (howler :: Howl.HOWLER | eff) Unit
playSound s = do
  let file = fileName s
  sound <- Howl.new (Howl.defaultProps { buffer = true, urls = [file] })
  Howl.play sound
