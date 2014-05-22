module Tests where

import Prelude
import Data.Maybe
import Data.Array
import Debug.Trace
import Control.Monad.Eff
import Data.Tuple

import Test.QuickCheck
import Test.QuickCheck.Tuple

import qualified Data.Map as M
import qualified Data.Set as S

import Data.Graph

instance arbMap :: (Eq k, Ord k, Arbitrary k, Arbitrary v) => Arbitrary (M.Map k v) where
  arbitrary = M.fromList <<< map runTestTuple <$> arbitrary

instance arbSet :: (Eq a, Ord a, Arbitrary a) => Arbitrary (S.Set a) where
  arbitrary = S.fromList <$> arbitrary

main = do
  -- Data.Map

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
  quickCheck $ \arr -> let f x = M.toList (M.fromList x) 
                           arr' = map runTestTuple arr
                       in f (f arr') == f (arr' :: [Tuple Number Number])

  trace "testFromListToList: fromList . toList = id"
  quickCheck $ \m -> let f m = M.fromList (M.toList m) in
                     M.toList (f m) == M.toList (m :: M.Map Number Number)
  
  trace "testUnionSymmetric: union m1 m2 == union m2 m1"
  quickCheck $ \m1 m2 -> let m3 = m1 `M.union` (m2 :: M.Map Number Number) in
                         let m4 = m2 `M.union` m1 in
                         M.toList m3 == M.toList m4

  trace "testUnionIdempotent"
  quickCheck $ \m1 m2 -> (m1 `M.union` m2) == ((m1 `M.union` m2) `M.union` (m2 :: M.Map Number Number))

  -- Data.Set

  trace "testMemberEmpty: member _ empty == false"
  quickCheck $ \a -> S.member a (S.empty :: S.Set Number) == false

  trace "testMemberSingleton: member a (singleton a) == true"
  quickCheck $ \a -> S.member (a :: Number) (S.singleton a) == true

  trace "testInsertDelete: member a (delete a (insert a empty) == false)"
  quickCheck $ \a -> (S.member (a :: Number) $ 
                          S.delete a $ 
                          S.insert a S.empty) == false

  trace "testSingletonToList: toList (singleton a) == [a]"
  quickCheck $ \a -> S.toList (S.singleton a :: S.Set Number) == [a]

  trace "testToListFromList: toList . fromList = id"
  quickCheck $ \arr -> let f x = S.toList (S.fromList x) in
                           f (f arr) == f (arr :: [Number])

  trace "testFromListToList: fromList . toList = id"
  quickCheck $ \s -> let f s = S.fromList (S.toList s) in
                     S.toList (f s) == S.toList (s :: S.Set Number)

  trace "testUnionSymmetric: union s1 s2 == union s2 s1"
  quickCheck $ \s1 s2 -> let s3 = s1 `S.union` (s2 :: S.Set Number) in
                         let s4 = s2 `S.union` s1 in
                         S.toList s3 == S.toList s4

  trace "testUnionIdempotent"
  quickCheck $ \s1 s2 -> (s1 `S.union` s2) == ((s1 `S.union` s2) `S.union` (s2 :: S.Set Number))

  -- Data.Graph

  trace "testOneVertex"
  quickCheck $ \v -> let g = Graph ([v] :: [Number]) [] in
                     let comps = scc g in
                     comps == [[v]]
  
  trace "testOneComponent"
  quickCheck $ \v1 v2 -> let g = Graph ([v1, v2] :: [Number]) [Edge v1 v2, Edge v2 v1] in
                         let comps = scc g in
                         comps == [[v1, v2]] || comps == [[v2, v1]]
  
  trace "testTwoComponents"
  quickCheck $ \v1 v2 -> let g = Graph ([v1, v2] :: [Number]) [] in
                         let comps = scc g in
                         comps == [[v1], [v2]] || comps == [[v2], [v1]]

  trace "testManyEdges"
  quickCheck $ \vs -> let g = Graph (vs :: [Number]) (Edge <$> vs <*> vs) in
                      vs == [] || length (scc g) == 1
  
  trace "testNoEdges"
  quickCheck $ \vs -> let g = Graph (vs :: [Number]) [] in
                      length (scc g) == length vs
  
  trace "testChain"
  quickCheck $ \vs -> let g = Graph (vs :: [Number]) (reverse $ chain vs) in
                      scc g == reverse (map singleton vs)

chain :: forall v. [v] -> [Edge v]
chain [] = []
chain [_] = []
chain (v : tail@(w : _)) = Edge v w : chain tail
