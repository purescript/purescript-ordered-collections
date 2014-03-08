module Main where

import Prelude (($), (==))
import Data.Map
import Data.Maybe
import Test.QuickCheck
import Debug.Trace
import Control.Monad.Eff
import Data.Tuple

main = do
  trace "testLookupEmpty: lookup _ empty == Nothing"
  quickCheck $ \k -> lookup k (empty :: Map Number Number) == Nothing

  trace "testLookupSingleton: lookup k (singleton k v) == Just v"
  quickCheck $ \k v -> lookup (k :: Number) (singleton k (v :: Number)) == Just v

  trace "testInsertTwo: lookup k (insert k v1 (insert k v2) empty) == Just v1"
  quickCheck $ \k v1 v2 -> (lookup (k :: Number) $ insert k (v1 :: Number) $ insert k (v2 :: Number) empty) == Just v1

  trace "testInsertDelete: lookup k (delete k (insert k v empty) = Nothing)"
  quickCheck $ \k v -> (lookup (k :: Number) $ delete k $ insert k (v :: Number) empty) == Nothing

  trace "testSingletonToList: toList (singleton k v) == [Tuple k v]"
  quickCheck $ \k v -> toList (singleton k v :: Map Number Number) == [Tuple k v]

