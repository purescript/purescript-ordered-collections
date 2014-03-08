module Main where

import Prelude (($), (==))
import Data.Map
import Data.Maybe
import Test.QuickCheck
import Debug.Trace
import Control.Monad.Eff

testLookupEmpty = do
  trace "testLookupEmpty: lookup _ empty == Nothing"
  quickCheck $ \k -> 
    case lookup (k :: Number) (empty :: Map Number Number) of
      Nothing -> true
      _ -> false

testLookupSingleton = do
  trace "testLookupSingleton: lookup k (singleton k v) == Just v"
  quickCheck $ \k v -> lookup (k :: Number) (singleton k (v :: Number)) == Just v

main = do
  testLookupEmpty
  testLookupSingleton

