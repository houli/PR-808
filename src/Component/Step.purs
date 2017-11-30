module Component.Step where

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Data.Function (const, ($))
import Data.Functor ((<$>))
import Data.HeytingAlgebra (not)
import Data.Lens (Lens, use, (.=))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.NaturalTransformation (type (~>))
import Data.Symbol (SProxy(..))
import Data.Unit (Unit)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State = { on :: Boolean }

on :: forall a b r. Lens { on :: a | r } { on :: b | r } a b
on = prop (SProxy :: SProxy "on")

data Query a = Toggle a
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
      on' <- use on
      let newState = not on'
      on .= newState
      H.raise $ NotifyToggled newState
      pure next
    IsOn reply -> do
      reply <$> use on
