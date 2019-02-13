module Data.Map
  ( module Data.Map.Internal
  , keys
  ) where

import Prelude

import Data.Map.Internal (Map, alter, checkValid, delete, empty, filter, filterKeys, filterWithKey, findMax, findMin, foldSubmap, fromFoldable, fromFoldableWith, fromFoldableWithIndex, insert, insertWith, isEmpty, isSubmap, lookup, lookupGE, lookupGT, lookupLE, lookupLT, member, pop, showTree, singleton, size, submap, toUnfoldable, toUnfoldableUnordered, union, unionWith, unions, intersection, intersectionWith, difference, update, values, mapMaybeWithKey, mapMaybe)
import Data.Set (Set)
import Unsafe.Coerce (unsafeCoerce)

keys :: forall k v. Map k v -> Set k
keys = (unsafeCoerce :: Map k Unit -> Set k) <<< void
