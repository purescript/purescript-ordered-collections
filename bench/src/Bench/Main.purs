module Bench.Main where

import Prelude

import Bench.Data.Map (benchMap)
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "Map"
  log "==="
  benchMap
