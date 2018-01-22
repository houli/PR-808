module PR808.Presets
  ( allPresets
  , fifteenStep
  ) where

import PR808.Types (Preset, Sound(..))

fifteenStep :: Preset
fifteenStep =
  { name: "15 Step"
  , bpm: 95
  , tracks:
    [ { sound: Kick
      , steps: [true, false, false, false, false, false, true, true, false, false]
      }
    , { sound: Snare
      , steps: [false, false, false, true, false, true, false, false, true, false]
      }
    , { sound: HiHatClosed
      , steps: [false, false, true, false, true, false, true, false, true, false]
      }
    , { sound: Clap
      , steps: [false, false, true, false, true, false, true, false, true, false, false, false, true, false, false, true, false, false, true, false]
      }
    ]
  }

lotusFlower :: Preset
lotusFlower =
  { name: "Lotus Flower"
  , bpm: 130
  , tracks:
    [ { sound: Kick
      , steps: [true, false, true, false, false, false, false, false]
      }
    , { sound: Snare
      , steps: [false, false, false, false, false, false, true, false, false, false, false, false, true, false, false, true]
      }
    , { sound: HiHatClosed
      , steps: [true, false, true, false]
      }
    ]
  }

allPresets :: Array Preset
allPresets = [fifteenStep, lotusFlower]
