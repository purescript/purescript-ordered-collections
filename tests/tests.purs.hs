module Main where

import Prelude
import Data.Maybe
import Data.Array
import Test.QuickCheck
import Debug.Trace
import Control.Monad.Eff
import Data.Tuple

import qualified Data.Map as M
import qualified Data.Set as S

instance arbMap :: (Eq k, Ord k, Arb k, Arb v) => Arb (M.Map k v) where
  arb = M.fromList <$> arb

main = do
  trace "testLookupEmpty: lookup _ empty == Nothing"
  quickCheck $ \k -> M.lookup k (M.empty :: M.Map Number Number) == Nothing

  trace "testLookupSingleton: lookup k (singleton k v) == Just v"
  quickCheck $ \k v -> M.lookup (k :: Number) (M.singleton k (v :: Number)) == Just v

  trace "testInsertTwo: lookup k (insert k v1 (insert k v2) empty) == Just v1"
  quickCheck $ \k v1 v2 -> (M.lookup (k :: Number) $ 
                              M.insert k (v1 :: Number) $ 
                              M.insert k (v2 :: Number) M.empty) == Just v1

  trace "testInsertDelete: lookup k (delete k (insert k v empty) = Nothing)"
  quickCheck $ \k v -> (M.lookup (k :: Number) $ 
                          M.delete k $ 
                          M.insert k (v :: Number) M.empty) == Nothing

  trace "testSingletonToList: toList (singleton k v) == [Tuple k v]"
  quickCheck $ \k v -> M.toList (M.singleton k v :: M.Map Number Number) == [Tuple k v]

  trace "testToListFromList: toList . fromList = id"
  quickCheck $ \arr -> let f x = M.toList (M.fromList x) in
                           f (f arr) == f (arr :: [Tuple Number Number])

  trace "testFromListToList: fromList . toList = id"
  quickCheck $ \m -> let f m = M.fromList (M.toList m) in
                     M.toList (f m) == M.toList (m :: M.Map Number Number)
  
  trace "testUnionSymmetric: union m1 m2 == union m2 m1"
  quickCheck $ \m1 m2 -> let m3 = m1 `M.union` (m2 :: M.Map Number Number) in
                         let m4 = m2 `M.union` m1 in
                         M.toList m3 == M.toList m4

  trace "testUnionIdempotent"
  quickCheck $ \m1 m2 -> (m1 `M.union` m2) == ((m1 `M.union` m2) `M.union` (m2 :: M.Map Number Number))
