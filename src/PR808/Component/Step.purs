module PR808.Component.Step where

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Data.Function (const, ($))
import Data.Functor ((<$>))
import Data.HeytingAlgebra (not)
import Data.Lens (use, (.=))
import Data.Maybe (Maybe(..))
import Data.NaturalTransformation (type (~>))
import Data.Unit (Unit)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import PR808.Lenses (_on)
import PR808.Types (StepState)

data Query a = Toggle a
             | Set Boolean a
             | IsOn (Boolean -> a)

type Input = Unit
data Message = NotifyToggled Boolean

step :: forall m. H.Component HH.HTML Query Input Message m
step =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: StepState
  initialState = { on: false }

  render :: StepState -> H.ComponentHTML Query
  render state =
    HH.button
      [ HE.onClick (HE.input_ Toggle) ]
      [ HH.text
          if state.on
            then "ON"
            else "OFF"
      ]

  eval :: Query ~> H.ComponentDSL StepState Query Message m
  eval = case _ of
    Toggle next -> do
      newState <- not <$> use _on
      _on .= newState
      H.raise $ NotifyToggled newState
      pure next

    Set newState next -> do
      _on .= newState
      pure next

    IsOn reply -> do
      reply <$> use _on
