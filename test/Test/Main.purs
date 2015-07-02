module Test.Main where

import Prelude

import Control.Monad.Eff.Console (log)

import Test.Data.Map (mapTests)
import Test.Data.StrMap (strMapTests)

main = do
  log "Running Map tests"
  mapTests

  log "Running StrMap tests"
  strMapTests
