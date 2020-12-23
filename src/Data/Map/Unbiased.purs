module Data.Map.Unbiased where

import Prelude

import Control.Alt (class Alt)
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Map.Internal as M
import Data.Newtype (class Newtype)
import Data.Ord (class Ord1)
import Data.Traversable (class Traversable)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Data.Tuple (Tuple)

-- | `Map k v` provides a `Semigroup` instance for `Map` whose definition
-- | depends on the underlying `value` type's `Semigroup` instance.
-- | You should only use this type when you need `Data.Map` to have
-- | a `Semigroup` instance.
-- |
-- | ```purescript
-- | let
-- |   s :: forall key value. key -> value -> Map key value
-- |   s k v = Unbiased.Map (Data.Map.singleton k v)
-- |
-- | (s 1     "foo") <> (s 1     "bar") == (s 1  "foobar")
-- | (s 1 (First 1)) <> (s 1 (First 2)) == (s 1 (First 1))
-- | (s 1  (Last 1)) <> (s 1  (Last 2)) == (s 1  (Last 1))
-- | ```
newtype Map k v = Map (M.Map k v)

type role Map nominal representational

derive newtype instance eq1Map :: Eq k => Eq1 (Map k)
derive newtype instance eqMap :: (Eq k, Eq v) => Eq (Map k v)
derive newtype instance ord1Map :: Ord k => Ord1 (Map k)
derive newtype instance ordMap :: (Ord k, Ord v) => Ord (Map k v)
derive instance newtypeMap :: Newtype (Map k v) _

instance showMap :: (Show k, Show v) => Show (Map k v) where
  show m = "(fromFoldable " <> show (toAscArray m) <> ")" where
    toAscArray :: Map k v -> Array (Tuple k v)
    toAscArray (Map m') = M.toUnfoldable m'

instance appendMap :: (Ord k, Semigroup v) => Semigroup (Map k v) where
  append (Map l) (Map r) = Map (M.unionWith append l r)

derive newtype instance monoidMap :: Ord k => Monoid (Map k v)
instance altMap :: Ord k => Alt (Map k) where
  alt (Map l) (Map r) = Map (M.union l r)

derive newtype instance functorMap :: Functor (Map k)
derive newtype instance functorWithIndexMap :: FunctorWithIndex k (Map k)
derive newtype instance foldableMap :: Foldable (Map k)
derive newtype instance foldableWithIndexMap :: FoldableWithIndex k (Map k)
derive newtype instance traversableMap :: Traversable (Map k)
derive newtype instance traversableWithIndexMap :: TraversableWithIndex k (Map k)
