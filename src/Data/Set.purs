-- | This module defines a type of sets as height-balanced (AVL) binary trees.
-- | Efficient set operations are implemented in terms of
-- | <https://www.cs.cmu.edu/~guyb/papers/BFS16.pdf>

module Data.Set
  ( Set
  , fromFoldable
  , toUnfoldable
  , empty
  , isEmpty
  , singleton
  , map
  , checkValid
  , insert
  , member
  , delete
  , toggle
  , size
  , findMin
  , findMax
  , union
  , unions
  , difference
  , subset
  , properSubset
  , intersection
  , filter
  , mapMaybe
  , catMaybes
  , toMap
  , fromMap
  ) where

import Prelude hiding (map)

import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.List (List)
import Data.List as List
import Data.Map.Internal as M
import Data.Maybe (Maybe(..), maybe)
import Data.Ord (class Ord1)
import Data.Unfoldable (class Unfoldable)
import Prelude as Prelude
import Safe.Coerce (coerce)

-- | `Set a` represents a set of values of type `a`
newtype Set a = Set (M.Map a Unit)

-- | Create a set from a foldable structure.
fromFoldable :: forall f a. Foldable f => Ord a => f a -> Set a
fromFoldable = foldl (\m a -> insert a m) empty

-- | Convert a set to an unfoldable structure.
toUnfoldable :: forall f a. Unfoldable f => Set a -> f a
toUnfoldable = List.toUnfoldable <<< toList

toList :: forall a. Set a -> List a
toList (Set m) = M.keys m

instance eqSet :: Eq a => Eq (Set a) where
  eq (Set m1) (Set m2) = m1 == m2

instance eq1Set :: Eq1 Set where
  eq1 = eq

instance showSet :: Show a => Show (Set a) where
  show s = "(fromFoldable " <> show (toUnfoldable s :: Array a) <> ")"

instance ordSet :: Ord a => Ord (Set a) where
  compare s1 s2 = compare (toList s1) (toList s2)

instance ord1Set :: Ord1 Set where
  compare1 = compare

instance monoidSet :: Ord a => Monoid (Set a) where
  mempty = empty

instance semigroupSet :: Ord a => Semigroup (Set a) where
  append = union

instance foldableSet :: Foldable Set where
  foldMap f = foldMap f <<< toList
  foldl f x = foldl f x <<< toList
  foldr f x = foldr f x <<< toList

-- | An empty set
empty :: forall a. Set a
empty = Set M.empty

-- | Test if a set is empty
isEmpty :: forall a. Set a -> Boolean
isEmpty = coerce (M.isEmpty :: M.Map a Unit -> _)

-- | Create a set with one element
singleton :: forall a. a -> Set a
singleton a = Set (M.singleton a unit)

-- | Maps over the values in a set.
-- |
-- | This operation is not structure-preserving for sets, so is not a valid
-- | `Functor`. An example case: mapping `const x` over a set with `n > 0`
-- | elements will result in a set with one element.
map :: forall a b. Ord b => (a -> b) -> Set a -> Set b
map f = foldl (\m a -> insert (f a) m) empty

-- | Check whether the underlying tree satisfies the height, size, and ordering invariants.
-- |
-- | This function is provided for internal use.
checkValid :: forall a. Ord a => Set a -> Boolean
checkValid = coerce (M.checkValid :: M.Map a Unit -> _)

-- | Test if a value is a member of a set
member :: forall a. Ord a => a -> Set a -> Boolean
member = coerce (M.member :: _ -> M.Map a Unit -> _)

-- | Insert a value into a set
insert :: forall a. Ord a => a -> Set a -> Set a
insert a (Set m) = Set (M.insert a unit m)

-- | Delete a value from a set
delete :: forall a. Ord a => a -> Set a -> Set a
delete = coerce (M.delete :: _ -> M.Map a Unit -> _)

-- | Insert a value into a set if it is not already present, if it is present, delete it.
toggle :: forall a. Ord a => a -> Set a -> Set a
toggle a (Set m) = Set (M.alter (maybe (Just unit) (\_ -> Nothing)) a m)

-- | Find the size of a set
size :: forall a. Set a -> Int
size = coerce (M.size :: M.Map a Unit -> _)

findMin :: forall a. Set a -> Maybe a
findMin (Set m) = Prelude.map _.key (M.findMin m)

findMax :: forall a. Set a -> Maybe a
findMax (Set m) = Prelude.map _.key (M.findMax m)

-- | Form the union of two sets
-- |
-- | Running time: `O(n + m)`
union :: forall a. Ord a => Set a -> Set a -> Set a
union = coerce (M.union :: M.Map a Unit -> _ -> _)

-- | Form the union of a collection of sets
unions :: forall f a. Foldable f => Ord a => f (Set a) -> Set a
unions = foldl union empty

-- | Form the set difference
difference :: forall a. Ord a => Set a -> Set a -> Set a
difference = coerce (M.difference :: M.Map a Unit -> M.Map a Unit -> _)

-- | True if and only if every element in the first set
-- | is an element of the second set
subset :: forall a. Ord a => Set a -> Set a -> Boolean
subset s1 s2 = isEmpty $ s1 `difference` s2

-- | True if and only if the first set is a subset of the second set
-- | and the sets are not equal
properSubset :: forall a. Ord a => Set a -> Set a -> Boolean
properSubset s1 s2 = size s1 /= size s2 && subset s1 s2

-- | The set of elements which are in both the first and second set
intersection :: forall a. Ord a => Set a -> Set a -> Set a
intersection = coerce (M.intersection :: M.Map a Unit -> M.Map a Unit -> _)

-- | Filter out those values of a set for which a predicate on the value fails
-- | to hold.
filter :: forall a. Ord a => (a -> Boolean) -> Set a -> Set a
filter = coerce (M.filterKeys :: _ -> M.Map a Unit -> _)

-- | Applies a function to each value in a set, discarding entries where the
-- | function returns `Nothing`.
mapMaybe :: forall a b. Ord b => (a -> Maybe b) -> Set a -> Set b
mapMaybe f = foldr (\a acc -> maybe acc (\b -> insert b acc) (f a)) empty

-- | Filter a set of optional values, discarding values that contain `Nothing`
catMaybes :: forall a. Ord a => Set (Maybe a) -> Set a
catMaybes = mapMaybe identity

-- | A set is a map with no value attached to each key.
toMap :: forall a. Set a -> M.Map a Unit
toMap (Set s) = s

-- | A map with no value attached to each key is a set.
-- | See also `Data.Map.keys`.
fromMap :: forall a. M.Map a Unit -> Set a
fromMap = Set
