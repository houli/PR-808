module Component.Track where

import Prelude

import Audio.Howler (HOWLER)
import Component.Step as Step
import Control.Monad.Aff (Aff)
import Data.Array ((!!))
import Data.Lens (Lens, use, (%=), (.=))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sound (Sound(..), allSounds, playSound)

type State = { sound :: Sound, playing :: Boolean }

sound :: forall a b r. Lens { sound :: a | r } { sound :: b | r } a b
sound = prop (SProxy :: SProxy "sound")

playing :: forall a b r. Lens { playing :: a | r } { playing :: b | r } a b
playing = prop (SProxy :: SProxy "playing")

data Query a = PlayPause a
             | ChangeSound Int a

type Input = Unit
type Message = Void

data Slot = StepSlot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

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
  initialState = { sound: Cowbell, playing: false }

  render :: State -> H.ParentHTML Query Step.Query Slot (Aff (howler :: HOWLER | eff))
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "PR-808" ]
      , HH.select
          [ HE.onSelectedIndexChange (HE.input ChangeSound), HP.value $ show state.sound ]
          (map (\sound -> HH.option_ [ HH.text $ show sound ]) allSounds)
      , HH.button
          [ HE.onClick (HE.input_ PlayPause) ]
          [ HH.text
              if state.playing
              then "Pause"
              else "Play"
          ]
      , HH.slot StepSlot Step.step unit absurd
      ]

  eval :: Query ~> H.ParentDSL State Query Step.Query Slot Message (Aff (howler :: HOWLER | eff))
  eval = case _ of
    PlayPause next -> do
      playing %= not
      maybeStepIsOn <- H.query StepSlot $ H.request Step.IsOn
      case maybeStepIsOn of
        Nothing -> pure unit
        Just stepIsOn -> when stepIsOn $ do
          sound' <- use sound
          H.liftEff $ playSound sound'
      pure next
    ChangeSound index next -> do
      case allSounds !! index of
        Nothing -> pure unit
        Just newSound -> sound .= newSound
      pure next
