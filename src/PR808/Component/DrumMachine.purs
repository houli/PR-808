module PR808.Component.DrumMachine where

import Control.Applicative (pure, when)
import Control.Bind (bind, discard, (>>=))
import Data.Array ((..), filter)
import Data.Eq ((/=), (==))
import Data.EuclideanRing ((/))
import Data.Foldable (length, traverse_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Function (const, ($))
import Data.Functor ((<$>))
import Data.HeytingAlgebra (not)
import Data.Lens (use, (%=), (+=), (.=), (<>=))
import Data.Maybe (Maybe(..))
import Data.NaturalTransformation (type (~>))
import Data.Ring ((-))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Timer (clearInterval, setInterval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query.EventSource as ES

import PR808.Component.Track as Track
import PR808.Lenses (_bpm, _nextTrackId, _playing, _tracks)
import PR808.Types (DrumMachineState, TrackId, Preset)
import PR808.Util (maxInt)

beatSource :: forall f m. MonadAff m => Int -> f ES.SubscribeStatus -> ES.EventSource f m
beatSource tempo = ES.eventSource_' \emit -> do
  intervalId <- H.liftEffect $ setInterval (1000 / (tempo / 15)) emit
  pure $ clearInterval intervalId

data Query a = PlayStop a
             | PlayLoop (ES.SubscribeStatus -> a)
             | AddTrack a
             | IncreaseBPM a
             | DecreaseBPM a
             | PresetSetup Preset a
             | HandleTrackMessage TrackId Track.Message a

type Input = Unit
type Message = Void

type Slot = TrackId

drumMachine :: H.Component HH.HTML Query Input Message Aff
drumMachine =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: DrumMachineState
  initialState = { playing: false, bpm: 120, tracks: [], nextTrackId: 0 }

  render :: DrumMachineState -> H.ParentHTML Query Track.Query Slot Aff
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "PR-808" ]
      , HH.button
          [ HE.onClick (HE.input_ DecreaseBPM) ]
          [ HH.text "DOWN TEMPO" ]
      , HH.span_
          [ HH.text $ show state.bpm <> " BPM"]
      , HH.button
          [ HE.onClick (HE.input_ IncreaseBPM) ]
          [ HH.text "UP TEMPO" ]
      , HH.button
          [ HE.onClick (HE.input_ PlayStop) ]
          [ HH.text
              if state.playing
                then "Stop"
                else "Play"
          ]
      , HH.button
          [ HE.onClick (HE.input_ AddTrack) ]
          [ HH.text "Add Track" ]
      , HH.div_ (renderTrack <$> state.tracks)
      ]

  renderTrack :: Slot -> H.ParentHTML Query Track.Query Slot Aff
  renderTrack trackId = HH.div_ [ HH.slot trackId Track.track unit $ HE.input (HandleTrackMessage trackId) ]

  eval :: Query ~> H.ParentDSL DrumMachineState Query Track.Query Slot Message Aff
  eval = case _ of
    PlayStop next -> do
      shouldPlay <- not <$> use _playing
      when shouldPlay do
        bpm <- use _bpm
        H.subscribe (beatSource bpm $ H.request PlayLoop)
      _playing .= shouldPlay
      pure next

    PlayLoop reply -> do
      playing <- use _playing
      if playing
        then do
          use _tracks >>= traverse_ \trackId ->
            H.query trackId $ H.action Track.NextBeat
          pure $ reply ES.Listening
        else do
          resetAllTracks
          pure $ reply ES.Done

    AddTrack next -> do
      nextTrackId <- use _nextTrackId
      _nextTrackId += 1
      _tracks <>= [nextTrackId]
      resetAllTracks
      pure next

    IncreaseBPM next -> do
      _bpm += 5
      _playing .= false
      pure next

    DecreaseBPM next -> do
      _bpm %= \n -> maxInt 5 (n - 5)
      _playing .= false
      pure next

    PresetSetup preset next -> do
      _tracks .= if length preset.tracks == 0 then [] else 0..(length preset.tracks - 1)
      _nextTrackId .= length preset.tracks
      _bpm .= preset.bpm
      _playing .= false
      forWithIndex_ preset.tracks \i presetTrack ->
        H.query i $ H.action $ Track.PresetTrackSetup presetTrack
      pure next

    HandleTrackMessage trackId msg next -> do
      case msg of
        Track.NotifyRemove -> do
          _tracks %= removeTrack trackId
          resetAllTracks
      pure next

resetAllTracks :: forall m. H.ParentDSL DrumMachineState Query Track.Query Slot Message m Unit
resetAllTracks = use _tracks >>= traverse_ \trackId ->
  H.query trackId $ H.action Track.ResetCurrentStep

removeTrack :: TrackId -> Array TrackId -> Array TrackId
removeTrack trackId tracksArray = filter (_ /= trackId) tracksArray
