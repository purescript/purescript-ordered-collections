module Data.Set.NonEmpty
  ( NonEmptySet
  , singleton
  , cons
  , fromSet
  , fromFoldable
  , fromFoldable1
  , toSet
  , toUnfoldable
  , toUnfoldable1
  , map
  , member
  , insert
  , delete
  , size
  , min
  , max
  , unionSet
  , difference
  , subset
  , properSubset
  , intersection
  , filter
  , mapMaybe
  ) where

import Prelude hiding (map)

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable)
import Data.Function.Uncurried (mkFn3)
import Data.List.NonEmpty (NonEmptyList)
import Data.Map.Internal as Internal
import Data.Maybe (Maybe(..), fromJust)
import Data.Ord (class Ord1)
import Data.Semigroup.Foldable (class Foldable1, foldMap1, foldr1, foldl1)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, class Unfoldable1, unfoldr1)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Safe.Coerce (coerce)

-- | `NonEmptySet a` represents a non-empty set of values of type `a`
newtype NonEmptySet a = NonEmptySet (Set a)

derive newtype instance eqNonEmptySet :: Eq a => Eq (NonEmptySet a)
derive newtype instance eq1NonEmptySet :: Eq1 NonEmptySet
derive newtype instance ordNonEmptySet :: Ord a => Ord (NonEmptySet a)
derive newtype instance ord1NonEmptySet :: Ord1 NonEmptySet
derive newtype instance semigroupNonEmptySet :: Ord a => Semigroup (NonEmptySet a)
derive newtype instance foldableNonEmptySet :: Foldable NonEmptySet

instance foldable1NonEmptySet :: Foldable1 NonEmptySet where
  foldMap1 f = foldMap1 f <<< (toUnfoldable1 :: forall a. NonEmptySet a -> NonEmptyList a)
  foldr1 f = foldr1 f <<< (toUnfoldable1 :: forall a. NonEmptySet a -> NonEmptyList a)
  foldl1 f = foldl1 f <<< (toUnfoldable1 :: forall a. NonEmptySet a -> NonEmptyList a)

instance showNonEmptySet :: Show a => Show (NonEmptySet a) where
  show s = "(fromFoldable1 " <> show (toUnfoldable1 s :: NonEmptyArray a) <> ")"

-- | Create a set with one element.
singleton :: forall a. a -> NonEmptySet a
singleton = coerce (Set.singleton :: a -> _)

-- | Creates a `NonEmptySet` from an item and a `Set`.
cons :: forall a. Ord a => a -> Set a -> NonEmptySet a
cons = coerce (Set.insert :: a -> _)

-- | Attempts to create a non-empty set from a possibly-empty set.
fromSet :: forall a. Set a -> Maybe (NonEmptySet a)
fromSet s = if Set.isEmpty s then Nothing else Just (NonEmptySet s)

-- | Create a set from a foldable structure.
fromFoldable :: forall f a. Foldable f => Ord a => f a -> Maybe (NonEmptySet a)
fromFoldable = fromSet <<< Set.fromFoldable

-- | Create a set from a non-empty foldable structure.
fromFoldable1 :: forall f a. Foldable1 f => Ord a => f a -> NonEmptySet a
fromFoldable1 = foldMap1 singleton

-- | Forgets the non-empty property of a set, giving a normal possibly-empty
-- | set.
toSet :: forall a. NonEmptySet a -> Set a
toSet (NonEmptySet s) = s

-- | Convert a set to an unfoldable structure.
toUnfoldable :: forall f a. Unfoldable f => NonEmptySet a -> f a
toUnfoldable = coerce (Set.toUnfoldable :: Set a -> f a)

-- | Convert a set to a non-empty unfoldable structure.
toUnfoldable1 :: forall f a. Unfoldable1 f => NonEmptySet a -> f a
toUnfoldable1 = unfoldr1 (stepNext <$> _) <<< stepHead <<< Internal.toMapIter <<< Set.toMap <<< coerce
  where
  stepHead = Internal.stepAscCps (mkFn3 \k _ next -> Tuple k next) \_ -> unsafeCrashWith "toUnfoldable1: impossible"
  stepNext = Internal.stepAscCps (mkFn3 \k _ next -> Just (Tuple k next)) \_ -> Nothing

-- | Maps over the values in a set.
-- |
-- | This operation is not structure-preserving for sets, so is not a valid
-- | `Functor`. An example case: mapping `const x` over a set with `n > 0`
-- | elements will result in a set with one element.
map :: forall a b. Ord b => (a -> b) -> NonEmptySet a -> NonEmptySet b
map = coerce (Set.map :: (a -> b) -> _)

-- | Test if a value is a member of a set.
member :: forall a. Ord a => a -> NonEmptySet a -> Boolean
member = coerce (Set.member :: a -> _)

-- | Insert a value into a set.
insert :: forall a. Ord a => a -> NonEmptySet a -> NonEmptySet a
insert = coerce (Set.insert :: a -> _)

-- | Delete a value from a non-empty set. If this would empty the set, the
-- | result is `Nothing`.
delete :: forall a. Ord a => a -> NonEmptySet a -> Maybe (NonEmptySet a)
delete a (NonEmptySet s) = fromSet (Set.delete a s)

-- | Find the size of a set.
size :: forall a. NonEmptySet a -> Int
size = coerce (Set.size :: Set a -> _)

-- | The minimum value in the set.
min :: forall a. NonEmptySet a -> a
min (NonEmptySet s) = unsafePartial (fromJust (Set.findMin s))

-- | The maximum value in the set.
max :: forall a. NonEmptySet a -> a
max (NonEmptySet s) = unsafePartial (fromJust (Set.findMax s))

-- | Form the union of a set and the non-empty set.
unionSet :: forall a. Ord a => Set a -> NonEmptySet a -> NonEmptySet a
unionSet = coerce (append :: _ -> Set a -> _)

-- | Form the set difference. `Nothing` if the first is a subset of the second.
difference :: forall a. Ord a => NonEmptySet a -> NonEmptySet a -> Maybe (NonEmptySet a)
difference (NonEmptySet s1) (NonEmptySet s2) = fromSet (Set.difference s1 s2)

-- | True if and only if every element in the first set is an element of the
-- | second set.
subset :: forall a. Ord a => NonEmptySet a -> NonEmptySet a -> Boolean
subset = coerce (Set.subset :: Set a -> _)

-- | True if and only if the first set is a subset of the second set and the
-- | sets are not equal.
properSubset :: forall a. Ord a => NonEmptySet a -> NonEmptySet a -> Boolean
properSubset = coerce (Set.properSubset :: Set a -> _)

-- | The set of elements which are in both the first and second set. `Nothing`
-- | if the sets are disjoint.
intersection :: forall a. Ord a => NonEmptySet a -> NonEmptySet a -> Maybe (NonEmptySet a)
intersection (NonEmptySet s1) (NonEmptySet s2) = fromSet (Set.intersection s1 s2)

-- | Filter out those values of a set for which a predicate on the value fails
-- | to hold.
filter :: forall a. Ord a => (a -> Boolean) -> NonEmptySet a -> Set a
filter = coerce (Set.filter :: _ -> Set a -> _)

-- | Applies a function to each value in a set, discarding entries where the
-- | function returns `Nothing`.
mapMaybe :: forall a b. Ord b => (a -> Maybe b) -> NonEmptySet a -> Set b
mapMaybe = coerce (Set.mapMaybe :: _ -> Set a -> Set b)
