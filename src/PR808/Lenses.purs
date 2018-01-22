module PR808.Lenses where

import Data.Lens (Lens)
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))

_on :: forall a b r. Lens { on :: a | r } { on :: b | r } a b
_on = prop (SProxy :: SProxy "on")

_sound :: forall a b r. Lens { sound :: a | r } { sound :: b | r } a b
_sound = prop (SProxy :: SProxy "sound")

_steps :: forall a b r. Lens { steps :: a | r } { steps :: b | r } a b
_steps = prop (SProxy :: SProxy "steps")

_currentStep :: forall a b r. Lens { currentStep :: a | r } { currentStep :: b | r } a b
_currentStep = prop (SProxy :: SProxy "currentStep")

_muted :: forall a b r. Lens { muted :: a | r } { muted :: b | r } a b
_muted = prop (SProxy :: SProxy "muted")

_playing :: forall a b r. Lens { playing :: a | r } { playing :: b | r } a b
_playing = prop (SProxy :: SProxy "playing")

_bpm :: forall a b r. Lens { bpm :: a | r } { bpm :: b | r } a b
_bpm = prop (SProxy :: SProxy "bpm")

_tracks :: forall a b r. Lens { tracks :: a | r } { tracks :: b | r } a b
_tracks = prop (SProxy :: SProxy "tracks")

_nextTrackId :: forall a b r. Lens { nextTrackId :: a | r } { nextTrackId :: b | r } a b
_nextTrackId = prop (SProxy :: SProxy "nextTrackId")
