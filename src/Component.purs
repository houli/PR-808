module Component where

import Prelude

import Audio.Howler (HOWLER)
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Sound (Sound(..), playSound)

data Query a = ToggleState a

type State = { on ∷ Boolean }

component ∷ ∀ e. H.Component HH.HTML Query Unit Void (Aff (howler ∷ HOWLER | e))
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState ∷ State
  initialState = { on: false }

  render ∷ State → H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "PR-808" ]
      , HH.button
          [ HE.onClick (HE.input_ ToggleState) ]
          [ HH.text
              if not state.on
              then "Play the cowbell"
              else "Play it again"
          ]
      ]

  eval ∷ Query ~> H.ComponentDSL State Query Void (Aff (howler ∷ HOWLER | e))
  eval = case _ of
    ToggleState next → do
      H.liftEff $ playSound Cowbell
      H.modify (\state → { on: not state.on })
      pure next
