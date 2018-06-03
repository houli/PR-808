module PR808.Component.Track where

import Control.Applicative (pure, when)
import Control.Bind (bind, discard, (=<<))
import Data.Array ((!!), (..))
import Data.Foldable (length)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Function (const, ($), (>>>))
import Data.Functor ((<#>), (<$>))
import Data.HeytingAlgebra (not, (&&))
import Data.Lens (use, (%=), (+=), (.=))
import Data.Maybe (Maybe(..))
import Data.NaturalTransformation (type (~>))
import Data.Ord ((>=))
import Data.Ring ((-))
import Data.Semiring ((+))
import Data.Show (show)
import Data.Unit (Unit, unit)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import PR808.Component.Step as Step
import PR808.Lenses (_currentStep, _muted, _sound, _steps)
import PR808.Sound (allSounds, playSound)
import PR808.Types (PresetTrack, Sound(Cowbell), TrackState)
import PR808.Util (maxInt)

data Query a = NextBeat a
             | ResetCurrentStep a
             | ChangeSound Int a
             | AddStep a
             | RemoveStep a
             | Remove a
             | ToggleMute a
             | PresetTrackSetup PresetTrack a
             | HandleStepMessage Slot Step.Message a

type Input = Unit

data Message = NotifyRemove

type Slot = Int

track :: H.Component HH.HTML Query Input Message Aff
track =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: TrackState
  initialState = { sound: Cowbell, steps: 16, currentStep: 1, muted: false }

  render :: forall m. TrackState -> H.ParentHTML Query Step.Query Slot m
  render state =
    HH.div_
      [ HH.button
          [ HE.onClick (HE.input_ Remove) ]
          [ HH.text "Remove track" ]
      , HH.button
          [ HE.onClick (HE.input_ ToggleMute) ]
          [ HH.text
              if state.muted
                then "Unmute track"
                else "Mute track"
          ]
      , HH.select
          [ HE.onSelectedIndexChange (HE.input ChangeSound), HP.value $ show state.sound ]
          (allSounds
            <#> show
            >>> HH.text
            >>> pure
            >>> HH.option_
          )
      , HH.button
          [ HE.onClick (HE.input_ AddStep) ]
          [ HH.text "+" ]
      , HH.button
          [ HE.onClick (HE.input_ RemoveStep) ]
          [ HH.text "-" ]
      , HH.span_ (renderStep <$> 1..state.steps)
      ]

  renderStep :: forall m. Slot -> H.ParentHTML Query Step.Query Slot m
  renderStep n = HH.slot n Step.step unit $ HE.input (HandleStepMessage n)

  eval :: Query ~> H.ParentDSL TrackState Query Step.Query Slot Message Aff
  eval = case _ of
    NextBeat next -> do
      currentStep <- use _currentStep
      maybeStepIsOn <- H.query currentStep $ H.request Step.IsOn
      muted <- use _muted
      case maybeStepIsOn of
        Nothing -> pure unit
        Just stepIsOn -> when (stepIsOn && not muted) $ playSound' =<< use _sound
      numSteps <- use _steps
      _currentStep %= \current -> if current >= numSteps then 1 else current + 1
      pure next

    ResetCurrentStep next -> do
      _currentStep .= 1
      pure next

    ChangeSound index next -> do
      case allSounds !! index of
        Nothing -> pure unit
        Just newSound -> _sound .= newSound
      pure next

    AddStep next -> do
      _steps += 1
      pure next

    RemoveStep next -> do
      _steps %= \n -> maxInt 1 (n - 1)
      pure next

    Remove next -> do
      H.raise NotifyRemove
      pure next

    ToggleMute next -> do
      _muted %= not
      pure next

    PresetTrackSetup presetTrack next -> do
      _sound .= presetTrack.sound
      _steps .= length presetTrack.steps
      _currentStep .= 0
      _muted .= false
      forWithIndex_ presetTrack.steps \i stepState ->
        H.query (i + 1) $ H.action $ Step.Set stepState
      pure next

    HandleStepMessage _ msg next -> do
      case msg of
        Step.NotifyToggled newState -> do
          muted <- use _muted
          when (not muted && newState) $ playSound' =<< use _sound
      pure next

-- TODO: Possibly refactor to also use MonadState
playSound' :: forall m. MonadAff m => Sound -> m Unit
playSound' sound = H.liftEffect $ playSound sound 1.0
