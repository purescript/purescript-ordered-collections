module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.Data.Map (mapTests)
import Test.Data.Set (setTests)

main :: Effect Unit
main = do
  log "Running Map tests"
  mapTests

  log "Running Set tests"
  setTests
