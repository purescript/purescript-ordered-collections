module Data.Map
  ( Map(),
    empty,
    singleton,
    insert,
    lookup,
    delete
  ) where

import Prelude ((<), (==), Eq, Ord)
import Data.Maybe

data Map k v = Leaf | Branch { key :: k, value :: v, left :: Map k v, right :: Map k v }

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

