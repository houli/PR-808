module Component.Track where

import Audio.Howler (HOWLER)
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Aff (Aff)
import Data.Array ((!!), (..))
import Data.Function (const, ($), (<<<))
import Data.Functor ((<$>))
import Data.HeytingAlgebra (not)
import Data.Lens (Lens, use, (%=), (+=), (.=))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.NaturalTransformation (type (~>))
import Data.Ord ((<=))
import Data.Ring ((-))
import Data.Show (show)
import Data.Symbol (SProxy(..))
import Data.Unit (Unit, unit)
import Data.Void (Void, absurd)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Component.Step as Step
import Sound (Sound(..), allSounds, playSound)

type State = { sound :: Sound, playing :: Boolean, steps :: Int }

sound :: forall a b r. Lens { sound :: a | r } { sound :: b | r } a b
sound = prop (SProxy :: SProxy "sound")

playing :: forall a b r. Lens { playing :: a | r } { playing :: b | r } a b
playing = prop (SProxy :: SProxy "playing")

steps :: forall a b r. Lens { steps :: a | r } { steps :: b | r } a b
steps = prop (SProxy :: SProxy "steps")

data Query a = PlayPause a
             | ChangeSound Int a
             | AddStep a
             | RemoveStep a

type Input = Unit
type Message = Void

type Slot = Int

track :: forall e. H.Component HH.HTML Query Input Message (Aff (howler :: HOWLER | e))
track =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { sound: Cowbell, playing: false, steps: 1 }

  render :: State -> H.ParentHTML Query Step.Query Slot (Aff (howler :: HOWLER | e))
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "PR-808" ]
      , HH.select
          [ HE.onSelectedIndexChange (HE.input ChangeSound), HP.value $ show state.sound ]
          (HH.option_ <<< pure <<< HH.text <<< show <$> allSounds)
      , HH.button
          [ HE.onClick (HE.input_ PlayPause) ]
          [ HH.text
              if state.playing
              then "Pause"
              else "Play"
          ]
      , HH.button
          [ HE.onClick (HE.input_ AddStep) ]
          [ HH.text "+" ]
      , HH.button
          [ HE.onClick (HE.input_ RemoveStep) ]
          [ HH.text "-" ]
      , HH.span_ (renderStep <$> 1..state.steps)
      ]

  renderStep n = HH.slot n Step.step unit absurd

  eval :: Query ~> H.ParentDSL State Query Step.Query Slot Message (Aff (howler :: HOWLER | e))
  eval = case _ of
    PlayPause next -> do
      playing %= not
      -- maybeStepIsOn <- H.query StepSlot $ H.request Step.IsOn
      -- case maybeStepIsOn of
      --   Nothing -> pure unit
      --   Just stepIsOn -> when stepIsOn do
      sound' <- use sound
      H.liftEff $ playSound sound' 1.0
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
