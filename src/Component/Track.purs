module Component.Track where

import Audio.Howler (HOWLER)
import Control.Applicative (pure, when)
import Control.Bind (bind, discard)
import Control.Monad.Aff (Aff)
import Data.Array ((!!), (..))
import Data.Function (const, ($), (>>>))
import Data.Functor ((<#>), (<$>))
import Data.Lens (Lens, use, (%=), (+=), (.=))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.NaturalTransformation (type (~>))
import Data.Ord ((<=), (>=))
import Data.Ring ((-))
import Data.Semiring ((+))
import Data.Show (show)
import Data.Symbol (SProxy(..))
import Data.Unit (Unit, unit)
import Data.Void (absurd)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Component.Step as Step
import Sound (Sound(Cowbell), allSounds, playSound)

type State = { sound :: Sound, steps :: Int, currentStep :: Int }

sound :: forall a b r. Lens { sound :: a | r } { sound :: b | r } a b
sound = prop (SProxy :: SProxy "sound")

steps :: forall a b r. Lens { steps :: a | r } { steps :: b | r } a b
steps = prop (SProxy :: SProxy "steps")

currentStep :: forall a b r. Lens { currentStep :: a | r } { currentStep :: b | r } a b
currentStep = prop (SProxy :: SProxy "currentStep")

data Query a = NextBeat a
             | ResetCurrentStep a
             | ChangeSound Int a
             | AddStep a
             | RemoveStep a
             | Remove a

type Input = Unit

data Message = NotifyRemove

type Slot = Int

track :: forall eff. H.Component HH.HTML Query Input Message (Aff (howler :: HOWLER | eff))
track =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { sound: Cowbell, steps: 16, currentStep: 1 }

  render :: forall m. State -> H.ParentHTML Query Step.Query Slot m
  render state =
    HH.div_
      [ HH.button
          [ HE.onClick (HE.input_ Remove) ]
          [ HH.text "Remove track" ]
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
  renderStep n = HH.slot n Step.step unit absurd

  eval :: Query ~> H.ParentDSL State Query Step.Query Slot Message (Aff (howler :: HOWLER | eff))
  eval = case _ of
    NextBeat next -> do
      currentStep' <- use currentStep
      maybeStepIsOn <- H.query currentStep' $ H.request Step.IsOn
      case maybeStepIsOn of
        Nothing -> pure unit
        Just stepIsOn -> when stepIsOn do
          sound' <- use sound
          H.liftEff $ playSound sound' 1.0
      numSteps <- use steps
      currentStep %= \current -> if current >= numSteps then 1 else current + 1
      pure next
    ResetCurrentStep next -> do
      currentStep .= 1
      pure next
    ChangeSound index next -> do
      case allSounds !! index of
        Nothing -> pure unit
        Just newSound -> sound .= newSound
      pure next
    AddStep next -> do
     steps += 1
     pure next
    RemoveStep next -> do
     steps %= \n -> if n <= 1 then 1 else n - 1
     pure next
    Remove next -> do
      H.raise NotifyRemove
      pure next
