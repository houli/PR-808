module Component.Step where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State = { on :: Boolean }

data Query a = Toggle a
             | IsOn (Boolean -> a)

type Input = Unit
type Message = Void

step :: forall m. H.Component HH.HTML Query Input Message m
step =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { on: false }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.button
      [ HE.onClick (HE.input_ Toggle) ]
      [ HH.text
        if state.on
        then "ON"
        else "OFF"
      ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    Toggle next -> do
      H.modify $ \state -> state { on = not state.on }
      pure next
    IsOn reply -> do
      on <- H.gets _.on
      pure $ reply on
