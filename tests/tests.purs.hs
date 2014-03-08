module Main where

import Prelude (($))
import Data.Map
import Data.Maybe
import Test.QuickCheck
import Debug.Trace
import Control.Monad.Eff

testLookupEmpty = do
  trace "testLookupEmpty: lookup _ empty returns Nothing"
  quickCheck $ \k -> 
    case lookup (k :: Number) (empty :: Map Number Number) of
      Nothing -> true
      _ -> false

main = do
  testLookupEmpty
