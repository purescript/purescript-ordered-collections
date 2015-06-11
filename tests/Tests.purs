module Tests where

import Prelude

import Control.Monad.Eff.Console (log)
import Test.QuickCheck

import Tests.Data.Map (mapTests)
import Tests.Data.StrMap (strMapTests)

main = do
  log "Running Map tests"
  mapTests

  log "Running StrMap tests"
  strMapTests
