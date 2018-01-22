module PR808.Component.PresetPicker where

import Control.Applicative (pure)
import Control.Bind (discard)
import Data.Array ((!!))
import Data.Function (const, ($), (>>>))
import Data.Functor ((<#>))
import Data.Maybe (Maybe(..))
import Data.NaturalTransformation (type (~>))
import Data.Unit (Unit, unit)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import PR808.Presets (allPresets, fifteenStep)
import PR808.Types (Preset)

data Query a = ChangePreset Int a

type Input = Unit
data Message = NotifyPresetChanged Preset

presetPicker :: forall m. H.Component HH.HTML Query Input Message m
presetPicker =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: Preset
  initialState = fifteenStep

  render :: Preset -> H.ComponentHTML Query
  render preset =
    HH.select
      [ HE.onSelectedIndexChange (HE.input ChangePreset), HP.value preset.name ]
      (allPresets
       <#> _.name
       >>> HH.text
       >>> pure
       >>> HH.option_
      )

  eval :: Query ~> H.ComponentDSL Preset Query Message m
  eval = case _ of
    ChangePreset index next -> do
      case allPresets !! index of
        Nothing -> pure unit
        Just newPreset -> H.raise $ NotifyPresetChanged newPreset
      pure next
