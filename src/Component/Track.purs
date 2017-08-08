module Component.Track where

import Prelude

import Audio.Howler (HOWLER)
import Component.Step as Step
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Sound (Sound(..), playSound)

type State = { sound :: Sound, playing :: Boolean }

data Query a = PlayPause a

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
      H.modify $ \state -> state { playing = not state.playing }
      stepIsOn <- H.query StepSlot $ H.request Step.IsOn
      case stepIsOn of
        Nothing -> pure unit
        Just stepIsOn -> when stepIsOn $ do
          sound <- H.gets _.sound
          H.liftEff $ playSound sound
      pure next
