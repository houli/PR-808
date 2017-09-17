module Component.DrumMachine where

import Audio.Howler (HOWLER)
import Control.Applicative (pure, when)
import Control.Bind (bind, discard, (>>=))
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff.Timer (TIMER, clearInterval, setInterval)
import Data.Array (filter)
import Data.Eq ((/=))
import Data.EuclideanRing ((/))
import Data.Foldable (traverse_)
import Data.Function (const, ($))
import Data.Functor ((<$>))
import Data.HeytingAlgebra (not)
import Data.Lens (Lens, use, (%=), (+=), (.=), (<>=))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.NaturalTransformation (type (~>))
import Data.Ord ((<=))
import Data.Ring ((-))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Symbol (SProxy(..))
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query.EventSource as ES

import Component.Track as Track

type TrackId = Int

type State = { playing :: Boolean, bpm :: Int, tracks :: Array TrackId, nextTrackId :: TrackId }

beatSource :: forall f m eff. MonadAff (avar :: AVAR, timer :: TIMER | eff) m => Int -> f ES.SubscribeStatus -> ES.EventSource f m
beatSource tempo = ES.eventSource_' \emit -> do
  intervalId <- H.liftEff $ setInterval (1000 / (tempo / 15)) emit
  pure $ clearInterval intervalId

playing :: forall a b r. Lens { playing :: a | r } { playing :: b | r } a b
playing = prop (SProxy :: SProxy "playing")

bpm :: forall a b r. Lens { bpm :: a | r } { bpm :: b | r } a b
bpm = prop (SProxy :: SProxy "bpm")

tracks :: forall a b r. Lens { tracks :: a | r } { tracks :: b | r } a b
tracks = prop (SProxy :: SProxy "tracks")

nextTrackId :: forall a b r. Lens { nextTrackId :: a | r } { nextTrackId :: b | r } a b
nextTrackId = prop (SProxy :: SProxy "nextTrackId")

data Query a = PlayStop a
             | PlayLoop (ES.SubscribeStatus -> a)
             | AddTrack a
             | IncreaseBPM a
             | DecreaseBPM a
             | HandleTrackMessage TrackId Track.Message a

type Input = Unit
type Message = Void

type Slot = TrackId

drumMachine :: forall eff. H.Component HH.HTML Query Input Message (Aff (howler :: HOWLER, avar :: AVAR, timer :: TIMER | eff))
drumMachine =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { playing: false, bpm: 120, tracks: [0], nextTrackId: 1 }

  render :: State -> H.ParentHTML Query Track.Query Slot (Aff (howler :: HOWLER, avar :: AVAR, timer :: TIMER | eff))
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

  renderTrack :: Slot -> H.ParentHTML Query Track.Query Slot (Aff (howler :: HOWLER, avar :: AVAR, timer :: TIMER | eff))
  renderTrack trackId = HH.div_ [ HH.slot trackId Track.track unit $ HE.input (HandleTrackMessage trackId) ]

  eval :: Query ~> H.ParentDSL State Query Track.Query Slot Message (Aff (howler :: HOWLER, avar :: AVAR, timer :: TIMER | eff))
  eval = case _ of
    PlayStop next -> do
      playing' <- use playing
      when (not playing') do
        bpm' <- use bpm
        H.subscribe (beatSource bpm' $ H.request PlayLoop)
      playing %= not
      pure next
    PlayLoop reply -> do
      playing' <- use playing
      if playing'
        then do
          use tracks >>= traverse_ \trackId ->
            H.query trackId $ H.action Track.NextBeat
          pure $ reply ES.Listening
        else do
          resetAllTracks
          pure $ reply ES.Done
    AddTrack next -> do
     nextTrackId' <- use nextTrackId
     nextTrackId += 1
     tracks <>= [nextTrackId']
     resetAllTracks
     pure next
    IncreaseBPM next -> do
     bpm += 5
     playing .= false
     pure next
    DecreaseBPM next -> do
     bpm %= \n -> if n <= 5 then 5 else n - 5
     playing .= false
     pure next
    HandleTrackMessage trackId msg next -> do
     case msg of
       Track.NotifyRemove -> do
         tracks %= removeTrack trackId
         resetAllTracks
     pure next

resetAllTracks :: forall m. H.ParentDSL State Query Track.Query Slot Message m Unit
resetAllTracks = use tracks >>= traverse_ \trackId ->
  H.query trackId $ H.action Track.ResetCurrentStep

removeTrack :: TrackId -> Array TrackId -> Array TrackId
removeTrack trackId tracksArray = filter (_ /= trackId) tracksArray
