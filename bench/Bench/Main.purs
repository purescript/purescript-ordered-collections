module Bench.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Bench.Data.Map (benchMap)
import Bench.Data.StrMap (benchStrMap)

main :: Eff (console :: CONSOLE) Unit
main = do
  log "Map"
  log "==="
  benchMap

  log ""


  log "StrMap"
  log "======"
  benchStrMap
