module PR808.Types where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Show (class Show)

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

-- Step component state

type StepState = { on :: Boolean }

-- Track component state

type TrackState = { sound :: Sound, steps :: Int, currentStep :: Int, muted :: Boolean }

-- Drum machine component state

type TrackId = Int

type DrumMachineState = { playing :: Boolean, bpm :: Int, tracks :: Array TrackId, nextTrackId :: TrackId }

-- Presets

type Preset = { bpm :: Int, name :: String, tracks :: Array PresetTrack }

type PresetTrack = { sound :: Sound, steps :: Array Boolean }
