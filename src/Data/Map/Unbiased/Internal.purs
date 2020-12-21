-- | This module defines a type of maps as balanced 2-3 trees, based on
-- | <http://www.cs.princeton.edu/~dpw/courses/cos326-12/ass/2-3-trees.pdf>

module Data.Map.Unbiased.Internal
  ( Map
  , showTree
  , empty
  , isEmpty
  , singleton
  , checkValid
  , insert
  , insertWith
  , lookup
  , lookupLE
  , lookupLT
  , lookupGE
  , lookupGT
  , findMin
  , findMax
  , foldSubmap
  , submap
  , fromFoldable
  , fromFoldableWith
  , fromFoldableWithIndex
  , toUnfoldable
  , toUnfoldableUnordered
  , delete
  , pop
  , member
  , alter
  , update
  , keys
  , values
  , union
  , unionWith
  , unions
  , intersection
  , intersectionWith
  , difference
  , isSubmap
  , size
  , filterWithKey
  , filterKeys
  , filter
  , mapMaybeWithKey
  , mapMaybe
  , catMaybes
  ) where

import Prelude

import Control.Alt (class Alt)
import Data.Eq (class Eq1)
import Data.Foldable (foldl, class Foldable)
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Map.Internal as M
import Data.Ord (class Ord1)
import Data.Traversable (class Traversable)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Data.Tuple (Tuple)
import Data.Unfoldable (class Unfoldable)
import Safe.Coerce (coerce)

-- | `Map k v` represents maps from keys of type `k` to values of type `v`.
newtype Map k v = Map (M.Map k v)

type role Map nominal representational

derive newtype instance eq1Map :: Eq k => Eq1 (Map k)
derive newtype instance eqMap :: (Eq k, Eq v) => Eq (Map k v)
derive newtype instance ord1Map :: Ord k => Ord1 (Map k)
derive newtype instance ordMap :: (Ord k, Ord v) => Ord (Map k v)

instance showMap :: (Show k, Show v) => Show (Map k v) where
  show m = "(fromFoldable " <> show (toAscArray m) <> ")" where
    toAscArray :: Map k v -> Array (Tuple k v)
    toAscArray = toUnfoldable

instance appendMap :: (Ord k, Semigroup v) => Semigroup (Map k v) where
  append = unionWith append

derive newtype instance monoidMap :: Ord k => Monoid (Map k v)
instance altMap :: Ord k => Alt (Map k) where
  alt = union

derive newtype instance functorMap :: Functor (Map k)
derive newtype instance functorWithIndexMap :: FunctorWithIndex k (Map k)
derive newtype instance foldableMap :: Foldable (Map k)
derive newtype instance foldableWithIndexMap :: FoldableWithIndex k (Map k)

asList :: forall k v. List (Tuple k v) -> List (Tuple k v)
asList = identity

derive newtype instance traversableMap :: Traversable (Map k)
derive newtype instance traversableWithIndexMap :: TraversableWithIndex k (Map k)

-- | Render a `Map` as a `String`
showTree :: forall k v. Show k => Show v => Map k v -> String
showTree (Map m) = M.showTree m

-- | An empty map
empty :: forall k v. Map k v
empty = Map M.empty

-- | Test if a map is empty
isEmpty :: forall k v. Map k v -> Boolean
isEmpty (Map m) = M.isEmpty m

-- | Create a map with one key/value pair
singleton :: forall k v. k -> v -> Map k v
singleton k v = Map (M.singleton k v)

-- | Check whether the underlying tree satisfies the 2-3 invariant
-- |
-- | This function is provided for internal use.
checkValid :: forall k v. Map k v -> Boolean
checkValid (Map m) = M.checkValid m

-- | Look up a value for the specified key
lookup :: forall k v. Ord k => k -> Map k v -> Maybe v
lookup k (Map m) = M.lookup k m

-- | Look up a value for the specified key, or the greatest one less than it
lookupLE :: forall k v. Ord k => k -> Map k v -> Maybe { key :: k, value :: v }
lookupLE k (Map m) = M.lookupLE k m

-- | Look up a value for the greatest key less than the specified key
lookupLT :: forall k v. Ord k => k -> Map k v -> Maybe { key :: k, value :: v }
lookupLT k (Map m) = M.lookupLT k m

-- | Look up a value for the specified key, or the least one greater than it
lookupGE :: forall k v. Ord k => k -> Map k v -> Maybe { key :: k, value :: v }
lookupGE k (Map m) = M.lookupGE k m

-- | Look up a value for the least key greater than the specified key
lookupGT :: forall k v. Ord k => k -> Map k v -> Maybe { key :: k, value :: v }
lookupGT k (Map m) = M.lookupGT k m

-- | Returns the pair with the greatest key
findMax :: forall k v. Map k v -> Maybe { key :: k, value :: v }
findMax (Map m) = M.findMax m

-- | Returns the pair with the least key
findMin :: forall k v. Map k v -> Maybe { key :: k, value :: v }
findMin (Map m) = M.findMin m

-- | Fold over the entries of a given map where the key is between a lower and
-- | an upper bound. Passing `Nothing` as either the lower or upper bound
-- | argument means that the fold has no lower or upper bound, i.e. the fold
-- | starts from (or ends with) the smallest (or largest) key in the map.
-- |
-- | ```purescript
-- | foldSubmap (Just 1) (Just 2) (\_ v -> [v])
-- |  (fromFoldable [Tuple 0 "zero", Tuple 1 "one", Tuple 2 "two", Tuple 3 "three"])
-- |  == ["one", "two"]
-- |
-- | foldSubmap Nothing (Just 2) (\_ v -> [v])
-- |  (fromFoldable [Tuple 0 "zero", Tuple 1 "one", Tuple 2 "two", Tuple 3 "three"])
-- |  == ["zero", "one", "two"]
-- | ```
foldSubmap :: forall k v m. Ord k => Monoid m => Maybe k -> Maybe k -> (k -> v -> m) -> Map k v -> m
foldSubmap kmin kmax f (Map m) = M.foldSubmap kmin kmax f m

-- | Returns a new map containing all entries of the given map which lie
-- | between a given lower and upper bound, treating `Nothing` as no bound i.e.
-- | including the smallest (or largest) key in the map, no matter how small
-- | (or large) it is. For example:
-- |
-- | ```purescript
-- | submap (Just 1) (Just 2)
-- |   (fromFoldable [Tuple 0 "zero", Tuple 1 "one", Tuple 2 "two", Tuple 3 "three"])
-- |   == fromFoldable [Tuple 1 "one", Tuple 2 "two"]
-- |
-- | submap Nothing (Just 2)
-- |   (fromFoldable [Tuple 0 "zero", Tuple 1 "one", Tuple 2 "two", Tuple 3 "three"])
-- |   == fromFoldable [Tuple 0 "zero", Tuple 1 "one", Tuple 2 "two"]
-- | ```
-- |
-- | The function is entirely specified by the following
-- | property:
-- |
-- | ```purescript
-- | Given any m :: Map k v, mmin :: Maybe k, mmax :: Maybe k, key :: k,
-- |   let m' = submap mmin mmax m in
-- |     if (maybe true (\min -> min <= key) mmin &&
-- |         maybe true (\max -> max >= key) mmax)
-- |       then lookup key m == lookup key m'
-- |       else not (member key m')
-- | ```
submap :: forall k v. Ord k => Maybe k -> Maybe k -> Map k v -> Map k v
submap kmin kmax (Map m) = Map (M.submap kmin kmax m)

-- | Test if a key is a member of a map
member :: forall k v. Ord k => k -> Map k v -> Boolean
member k (Map m) = M.member k m

-- | Insert or replace a key/value pair in a map
insert :: forall k v. Ord k => k -> v -> Map k v -> Map k v
insert k v (Map m) = Map (M.insert k v m)

-- | Inserts or updates a value with the given function.
-- |
-- | The combining function is called with the existing value as the first
-- | argument and the new value as the second argument.
insertWith :: forall k v. Ord k => (v -> v -> v) -> k -> v -> Map k v -> Map k v
insertWith f k v (Map m) = Map (M.insertWith f k v m)

-- | Delete a key and its corresponding value from a map.
delete :: forall k v. Ord k => k -> Map k v -> Map k v
delete k (Map m) = Map (M.delete k m)

-- | Delete a key and its corresponding value from a map, returning the value
-- | as well as the subsequent map.
pop :: forall k v. Ord k => k -> Map k v -> Maybe (Tuple v (Map k v))
pop k (Map m) = wrap (M.pop k m)
  where
  wrap :: Maybe (Tuple v (M.Map k v)) -> Maybe (Tuple v (Map k v))
  wrap = coerce

-- | Insert the value, delete a value, or update a value for a key in a map
alter :: forall k v. Ord k => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
alter f k (Map m) = Map (M.alter f k m)

-- | Update or delete the value for a key in a map
update :: forall k v. Ord k => (v -> Maybe v) -> k -> Map k v -> Map k v
update f k (Map m) = Map (M.update f k m)

-- | Convert any foldable collection of key/value pairs to a map.
-- | On key collision, later values take precedence over earlier ones.
fromFoldable :: forall f k v. Ord k => Foldable f => f (Tuple k v) -> Map k v
fromFoldable xs = Map (M.fromFoldable xs)

-- | Convert any foldable collection of key/value pairs to a map.
-- | On key collision, the values are configurably combined.
fromFoldableWith :: forall f k v. Ord k => Foldable f => (v -> v -> v) -> f (Tuple k v) -> Map k v
fromFoldableWith f xs = Map (M.fromFoldableWith f xs)

-- | Convert any indexed foldable collection into a map.
fromFoldableWithIndex :: forall f k v. Ord k => FoldableWithIndex k f => f v -> Map k v
fromFoldableWithIndex = Map <<< M.fromFoldableWithIndex

-- | Convert a map to an unfoldable structure of key/value pairs where the keys are in ascending order
toUnfoldable :: forall f k v. Unfoldable f => Map k v -> f (Tuple k v)
toUnfoldable (Map m) = M.toUnfoldable m

-- | Convert a map to an unfoldable structure of key/value pairs
-- |
-- | While this traversal is up to 10% faster in benchmarks than `toUnfoldable`,
-- | it leaks the underlying map stucture, making it only suitable for applications
-- | where order is irrelevant.
-- |
-- | If you are unsure, use `toUnfoldable`
toUnfoldableUnordered :: forall f k v. Unfoldable f => Map k v -> f (Tuple k v)
toUnfoldableUnordered (Map m) = M.toUnfoldableUnordered m

-- | Get a list of the keys contained in a map
keys :: forall k v. Map k v -> List k
keys (Map m) = M.keys m

-- | Get a list of the values contained in a map
values :: forall k v. Map k v -> List v
values (Map m) = M.values m

-- | Compute the union of two maps, using the specified function
-- | to combine values for duplicate keys.
unionWith :: forall k v. Ord k => (v -> v -> v) -> Map k v -> Map k v -> Map k v
unionWith f (Map m1) (Map m2) = Map (M.unionWith f m1 m2)

-- | Compute the union of two maps, preferring values from the first map in the case
-- | of duplicate keys
union :: forall k v. Ord k => Map k v -> Map k v -> Map k v
union = unionWith const

-- | Compute the union of a collection of maps
unions :: forall k v f. Ord k => Foldable f => f (Map k v) -> Map k v
unions = foldl union empty

-- | Compute the intersection of two maps, using the specified function
-- | to combine values for duplicate keys.
intersectionWith :: forall k a b c. Ord k => (a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWith f (Map m1) (Map m2) = Map (M.intersectionWith f m1 m2)

-- | Compute the intersection of two maps, preferring values from the first map in the case
-- | of duplicate keys.
intersection :: forall k a b. Ord k => Map k a -> Map k b -> Map k a
intersection = intersectionWith const

-- | Difference of two maps. Return elements of the first map where
-- | the keys do not exist in the second map.
difference :: forall k v w. Ord k => Map k v -> Map k w -> Map k v
difference (Map m1) (Map m2) = Map (M.difference m1 m2)

-- | Test whether one map contains all of the keys and values contained in another map
isSubmap :: forall k v. Ord k => Eq v => Map k v -> Map k v -> Boolean
isSubmap (Map m1) (Map m2) = M.isSubmap m1 m2

-- | Calculate the number of key/value pairs in a map
size :: forall k v. Map k v -> Int
size (Map m) = M.size m

-- | Filter out those key/value pairs of a map for which a predicate
-- | fails to hold.
filterWithKey :: forall k v. Ord k => (k -> v -> Boolean) -> Map k v -> Map k v
filterWithKey predicate (Map m) = Map (M.filterWithKey predicate m)

-- | Filter out those key/value pairs of a map for which a predicate
-- | on the key fails to hold.
filterKeys :: forall k. Ord k => (k -> Boolean) -> Map k ~> Map k
filterKeys predicate (Map m) = Map (M.filterKeys predicate m)

-- | Filter out those key/value pairs of a map for which a predicate
-- | on the value fails to hold.
filter :: forall k v. Ord k => (v -> Boolean) -> Map k v -> Map k v
filter predicate (Map m) = Map (M.filter predicate m)

-- | Applies a function to each key/value pair in a map, discarding entries
-- | where the function returns `Nothing`.
mapMaybeWithKey :: forall k a b. Ord k => (k -> a -> Maybe b) -> Map k a -> Map k b
mapMaybeWithKey f (Map m) = Map (M.mapMaybeWithKey f m)

-- | Applies a function to each value in a map, discarding entries where the
-- | function returns `Nothing`.
mapMaybe :: forall k a b. Ord k => (a -> Maybe b) -> Map k a -> Map k b
mapMaybe f (Map m) = Map (M.mapMaybe f m)

-- | Filter a map of optional values, keeping only the key/value pairs which
-- | contain a value, creating a new map.
catMaybes :: forall k v. Ord k => Map k (Maybe v) -> Map k v
catMaybes (Map m) = Map (M.catMaybes m)
