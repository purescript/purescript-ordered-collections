module Data.Map
  ( Map(),
    empty,
    singleton,
    insert,
    lookup,
    delete,
    toList,
    fromList,
    union,
    map
  ) where

import qualified Prelude as P

import Data.Array (concat)
import Data.Foldable (foldl)
import Data.Maybe
import Data.Tuple

data Map k v = Leaf | Branch { key :: k, value :: v, left :: Map k v, right :: Map k v }

instance eqMap :: (P.Eq k, P.Eq v) => P.Eq (Map k v) where
  (==) m1 m2 = toList m1 P.== toList m2
  (/=) m1 m2 = P.not (m1 P.== m2)

instance showMap :: (P.Show k, P.Show v) => P.Show (Map k v) where
  show m = "fromList " P.++ P.show (toList m)

empty :: forall k v. Map k v
empty = Leaf

singleton :: forall k v. k -> v -> Map k v
singleton k v = Branch { key: k, value: v, left: empty, right: empty }

insert :: forall k v. (P.Eq k, P.Ord k) => k -> v -> Map k v -> Map k v
insert k v Leaf = singleton k v
insert k v (Branch b@{ key = k1 }) | k P.== k1 = Branch (b { key = k, value = v })
insert k v (Branch b@{ key = k1 }) | k P.< k1 = Branch (b { left = insert k v b.left })
insert k v (Branch b) = Branch (b { right = insert k v b.right })

lookup :: forall k v. (P.Eq k, P.Ord k) => k -> Map k v -> Maybe v
lookup k Leaf = Nothing
lookup k (Branch { key = k1, value = v }) | k P.== k1 = Just v
lookup k (Branch { key = k1, left = left }) | k P.< k1 = lookup k left
lookup k (Branch { right = right }) = lookup k right

findMinKey :: forall k v. (P.Ord k) => Map k v -> k
findMinKey (Branch { key = k, left = Leaf }) = k
findMinKey (Branch b) = findMinKey b.left

delete :: forall k v. (P.Eq k, P.Ord k) => k -> Map k v -> Map k v
delete k Leaf = Leaf
delete k (Branch b@{ key = k1, left = Leaf }) | k P.== k1 =
  case b of
    { left = Leaf } -> b.right
    { right = Leaf } -> b.left
    _ -> let minKey = findMinKey b.right in
         Branch (b { key = minKey, right = delete minKey b.right })
delete k (Branch b@{ key = k1 }) | k P.< k1 = Branch (b { left = delete k b.left })
delete k (Branch b) = Branch (b { right = delete k b.right })

toList :: forall k v. Map k v -> [Tuple k v]
toList Leaf = []
toList (Branch b) = toList b.left `concat` [Tuple b.key b.value] `concat` toList b.right

fromList :: forall k v. (P.Eq k, P.Ord k) => [Tuple k v] -> Map k v
fromList = foldl (\m (Tuple k v) -> insert k v m) empty

union :: forall k v. (P.Eq k, P.Ord k) => Map k v -> Map k v -> Map k v
union m1 m2 = foldl (\m (Tuple k v) -> insert k v m) m2 (toList m1)

map :: forall k v1 v2. (P.Eq k, P.Ord k) => (v1 -> v2) -> Map k v1 -> Map k v2
map _ Leaf = Leaf
map f (Branch b) = Branch (b { value = f b.value, left = map f b.left, right = map f b.right })
