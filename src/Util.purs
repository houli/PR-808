module Util
  ( maxInt
  ) where

import Data.Function (($))
import Data.Int (round, toNumber)
import Math (max)

maxInt :: Int -> Int -> Int
maxInt x y = round $ max (toNumber x) (toNumber y)
