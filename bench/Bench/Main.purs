module Bench.Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Unit (Unit)

import Bench.Data.Map (benchMap)

main :: Eff (console :: CONSOLE) Unit
main = benchMap
