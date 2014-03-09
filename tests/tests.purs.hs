module Main where

import Prelude
import Data.Maybe
import Data.Array
import Test.QuickCheck
import Debug.Trace
import Control.Monad.Eff
import Data.Tuple

instance showMap :: (Show k, Show v) => Show (Data.Map.Map k v) where
  show = Data.Map.showMap

instance eqMap :: (Eq k, Eq v) => Eq (Data.Map.Map k v) where
  (==) = Data.Map.eqMap
  (/=) m1 m2 = not (Data.Map.eqMap m1 m2)

instance arbMap :: (Eq k, Ord k, Arb k, Arb v) => Arb (Data.Map.Map k v) where
  arb = Data.Map.fromList <$> arb

main = do
  trace "testLookupEmpty: lookup _ empty == Nothing"
  quickCheck $ \k -> Data.Map.lookup k (Data.Map.empty :: Data.Map.Map Number Number) == Nothing

  trace "testLookupSingleton: lookup k (singleton k v) == Just v"
  quickCheck $ \k v -> Data.Map.lookup (k :: Number) (Data.Map.singleton k (v :: Number)) == Just v

  trace "testInsertTwo: lookup k (insert k v1 (insert k v2) empty) == Just v1"
  quickCheck $ \k v1 v2 -> (Data.Map.lookup (k :: Number) $ 
                              Data.Map.insert k (v1 :: Number) $ 
                              Data.Map.insert k (v2 :: Number) Data.Map.empty) == Just v1

  trace "testInsertDelete: lookup k (delete k (insert k v empty) = Nothing)"
  quickCheck $ \k v -> (Data.Map.lookup (k :: Number) $ 
                          Data.Map.delete k $ 
                          Data.Map.insert k (v :: Number) Data.Map.empty) == Nothing

  trace "testSingletonToList: toList (singleton k v) == [Tuple k v]"
  quickCheck $ \k v -> Data.Map.toList (Data.Map.singleton k v :: Data.Map.Map Number Number) == [Tuple k v]

  trace "testToListFromList: toList . fromList = id"
  quickCheck $ \arr -> let f x = Data.Map.toList (Data.Map.fromList x) in
                           f (f arr) == f (arr :: [Tuple Number Number])

  trace "testFromListToList: fromList . toList = id"
  quickCheck $ \m -> let f m = Data.Map.fromList (Data.Map.toList m) in
                     Data.Map.toList (f m) == Data.Map.toList (m :: Data.Map.Map Number Number)
  
  trace "testUnionSymmetric: union m1 m2 == union m2 m1"
  quickCheck $ \m1 m2 -> let m3 = m1 `Data.Map.union` (m2 :: Data.Map.Map Number Number) in
                         let m4 = m2 `Data.Map.union` m1 in
                         Data.Map.toList m3 == Data.Map.toList m4

  trace "testUnionIdempotent"
  quickCheck $ \m1 m2 -> (m1 `Data.Map.union` m2) == ((m1 `Data.Map.union` m2) `Data.Map.union` (m2 :: Data.Map.Map Number Number))
