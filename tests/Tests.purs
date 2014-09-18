module Tests where

import Debug.Trace
import Test.QuickCheck

import Tests.Data.Map (mapTests)
import Tests.Data.StrMap (strMapTests)

main = do
  trace "Running Map tests"
  mapTests

  trace "Running StrMap tests"
  strMapTests
