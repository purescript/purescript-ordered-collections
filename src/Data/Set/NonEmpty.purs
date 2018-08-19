module Data.Set.NonEmpty
  ( NonEmptySet
  , fromSet
  , toSet
  , fromFoldable
  , fromFoldable1
  , toUnfoldable
  , toUnfoldable1
  , singleton
  , map
  , member
  , insert
  , delete
  , size
  , findMin
  , findMax
  , unionSet
  , unions
  , difference
  , subset
  , properSubset
  , intersection
  ) where

import Prelude hiding (map)

import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldl)
import Data.List (List, (:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..), fromJust)
import Data.Ord (class Ord1)
import Data.Semigroup.Foldable (class Foldable1, foldMap1)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, class Unfoldable1, unfoldr1)
import Partial.Unsafe (unsafePartial)

-- | `NonEmptySet a` represents a non-empty set of values of type `a`
newtype NonEmptySet a = NonEmptySet (Set a)

-- | Attempts to create a non-empty set from a possibly-empty set.
fromSet :: forall a. Set a -> Maybe (NonEmptySet a)
fromSet s = if Set.isEmpty s then Nothing else Just (NonEmptySet s)

-- | Forgets the non-empty property of a set, giving a normal possibly-empty
-- | set.
toSet :: forall a. NonEmptySet a -> Set a
toSet (NonEmptySet s) = s

-- | Create a set from a foldable structure.
fromFoldable :: forall f a. Foldable f => Ord a => f a -> Maybe (NonEmptySet a)
fromFoldable = fromSet <<< Set.fromFoldable

-- | Create a set from a non-empty foldable structure.
fromFoldable1 :: forall f a. Foldable1 f => Ord a => f a -> NonEmptySet a
fromFoldable1 = foldMap1 singleton

-- | Convert a set to an unfoldable structure.
toUnfoldable :: forall f a. Unfoldable f => NonEmptySet a -> f a
toUnfoldable (NonEmptySet s) = Set.toUnfoldable s

-- | Convert a set to a non-empty unfoldable structure.
toUnfoldable1 :: forall f a. Unfoldable1 f => NonEmptySet a -> f a
toUnfoldable1 (NonEmptySet s) = unfoldr1 go (Set.toUnfoldable s :: List a)
  where
    go = unsafePartial case _ of
      x : List.Nil -> Tuple x Nothing
      x : tail -> Tuple x (Just tail)

derive newtype instance eqNonEmptySet :: Eq a => Eq (NonEmptySet a)
derive newtype instance eq1NonEmptySet :: Eq1 NonEmptySet
derive newtype instance ordNonEmptySet :: Ord a => Ord (NonEmptySet a)
derive newtype instance ord1NonEmptySet :: Ord1 NonEmptySet
derive newtype instance semigroupNonEmptySet :: Ord a => Semigroup (NonEmptySet a)
derive newtype instance foldableNonEmptySet :: Foldable NonEmptySet

instance foldable1NonEmptySet :: Foldable1 NonEmptySet where
  foldMap1 f = foldMap1 f <<< toUnfoldable1 :: forall a. NonEmptySet a -> NonEmptyList a
  fold1 = foldMap1 identity

instance showNonEmptySet :: Show a => Show (NonEmptySet a) where
  show s = "(fromFoldable " <> show (toUnfoldable1 s :: NonEmptyList a) <> ")"

-- | Create a set with one element.
singleton :: forall a. a -> NonEmptySet a
singleton a = NonEmptySet (Set.singleton a)

-- | Maps over the values in a set.
-- |
-- | This operation is not structure-preserving for sets, so is not a valid
-- | `Functor`. An example case: mapping `const x` over a set with `n > 0`
-- | elements will result in a set with one element.
map :: forall a b. Ord b => (a -> b) -> NonEmptySet a -> NonEmptySet b
map f (NonEmptySet s) = NonEmptySet (Set.map f s)

-- | Test if a value is a member of a set.
member :: forall a. Ord a => a -> NonEmptySet a -> Boolean
member a (NonEmptySet m) = Set.member a m

-- | Insert a value into a set.
insert :: forall a. Ord a => a -> NonEmptySet a -> NonEmptySet a
insert a (NonEmptySet s) = NonEmptySet (Set.insert a s)

-- | Delete a value from a non-empty set. If this would empty the set, the
-- | result is `Nothing`.
delete :: forall a. Ord a => a -> NonEmptySet a -> Maybe (NonEmptySet a)
delete a (NonEmptySet s) = fromSet (Set.delete a s)

-- | Find the size of a set.
size :: forall a. NonEmptySet a -> Int
size (NonEmptySet s) = Set.size s

-- | The minimum value in the set.
findMin :: forall a. NonEmptySet a -> a
findMin (NonEmptySet s) = unsafePartial (fromJust (Set.findMin s))

-- | The maximum value in the set.
findMax :: forall a. NonEmptySet a -> a
findMax (NonEmptySet s) = unsafePartial (fromJust (Set.findMax s))

-- | Form the union of a set and the non-empty set.
unionSet :: forall a. Ord a => Set.Set a -> NonEmptySet a -> NonEmptySet a
unionSet s1 (NonEmptySet s2) = NonEmptySet (s1 <> s2)

-- | Form the union of a non-empty collection of non-empty sets.
unions :: forall f a. Foldable1 f => Ord a => f (NonEmptySet a) -> NonEmptySet a
unions = foldl append (NonEmptySet Set.empty)

-- | Form the set difference. `Nothing` if the sets are identical.
difference :: forall a. Ord a => NonEmptySet a -> NonEmptySet a -> Maybe (NonEmptySet a)
difference (NonEmptySet s1) (NonEmptySet s2) = fromSet (Set.difference s1 s2)

-- | True if and only if every element in the first set is an element of the
-- | second set.
subset :: forall a. Ord a => NonEmptySet a -> NonEmptySet a -> Boolean
subset (NonEmptySet s1) (NonEmptySet s2) = Set.subset s1 s2

-- | True if and only if the first set is a subset of the second set and the
-- | sets are not equal.
properSubset :: forall a. Ord a => NonEmptySet a -> NonEmptySet a -> Boolean
properSubset (NonEmptySet s1) (NonEmptySet s2) = Set.properSubset s1 s2

-- | The set of elements which are in both the first and second set. `Nothing`
-- | if the sets are disjoint.
intersection :: forall a. Ord a => NonEmptySet a -> NonEmptySet a -> Maybe (NonEmptySet a)
intersection (NonEmptySet s1) (NonEmptySet s2) = fromSet (Set.intersection s1 s2)
