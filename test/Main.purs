module Test.Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Unit (Unit)

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = do
  log "You should add some tests."
