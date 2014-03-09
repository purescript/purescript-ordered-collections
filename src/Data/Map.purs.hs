module Data.Map
  ( Map(),
    eqMap,
    showMap,
    empty,
    singleton,
    insert,
    lookup,
    delete,
    toList,
    fromList,
    union
  ) where

import Prelude ((<), (==), (/=), (++), Show, Eq, Ord, not, show)
import Data.Maybe
import Data.Tuple
import Data.Array (concat, foldl)

data Map k v = Leaf | Branch { key :: k, value :: v, left :: Map k v, right :: Map k v }

instance eqMapI :: (Eq k, Eq v) => Eq (Map k v) where
  (==) = eqMap
  (/=) m1 m2 = not (m1 `eqMap` m2)

eqMap :: forall k v. (Eq k, Eq v) => Map k v -> Map k v -> Boolean
eqMap m1 m2 = toList m1 == toList m2

instance showMapI :: (Show k, Show v) => Show (Map k v) where
  show = showMap

showMap :: forall k v. (Show k, Show v) => Map k v -> String
showMap m = "fromList " ++ show (toList m)

empty :: forall k v. Map k v
empty = Leaf

singleton :: forall k v. k -> v -> Map k v
singleton k v = Branch { key: k, value: v, left: empty, right: empty }

insert :: forall k v. (Eq k, Ord k) => k -> v -> Map k v -> Map k v
insert k v Leaf = singleton k v
insert k v (Branch b@{ key = k1 }) | k == k1 = Branch (b { key = k, value = v })
insert k v (Branch b@{ key = k1 }) | k < k1 = Branch (b { left = insert k v b.left }) 
insert k v (Branch b) = Branch (b { right = insert k v b.right })

lookup :: forall k v. (Eq k, Ord k) => k -> Map k v -> Maybe v
lookup k Leaf = Nothing 
lookup k (Branch { key = k1, value = v }) | k == k1 = Just v
lookup k (Branch { key = k1, left = left }) | k < k1 = lookup k left
lookup k (Branch { right = right }) = lookup k right

findMinKey :: forall k v. (Ord k) => Map k v -> k
findMinKey (Branch { key = k, left = Leaf }) = k
findMinKey (Branch b) = findMinKey b.left

delete :: forall k v. (Eq k, Ord k) => k -> Map k v -> Map k v
delete k Leaf = Leaf
delete k (Branch b@{ key = k1, left = Leaf }) | k == k1 = 
  case b of
    { left = Leaf } -> b.right
    { right = Leaf } -> b.left
    _ -> let minKey = findMinKey b.right in
         Branch (b { key = minKey, right = delete minKey b.right })
delete k (Branch b@{ key = k1 }) | k < k1 = Branch (b { left = delete k b.left })
delete k (Branch b) = Branch (b { right = delete k b.right })

toList :: forall k v. Map k v -> [Tuple k v]
toList Leaf = []
toList (Branch b) = toList b.left `concat` [Tuple b.key b.value] `concat` toList b.right 

fromList :: forall k v. (Eq k, Ord k) => [Tuple k v] -> Map k v
fromList = foldl (\m (Tuple k v) -> insert k v m) empty

union :: forall k v. (Eq k, Ord k) => Map k v -> Map k v -> Map k v
union m1 m2 = foldl (\m (Tuple k v) -> insert k v m) m2 (toList m1)
