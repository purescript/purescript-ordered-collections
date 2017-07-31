-- | This module defines a type of native Javascript maps which
-- | require the keys to be strings.
-- |
-- | To maximize performance, Javascript objects are not wrapped,
-- | and some native code is used even when it's not necessary.

module Data.StrMap
  ( StrMap
  , empty
  , isEmpty
  , size
  , singleton
  , insert
  , lookup
  , toUnfoldable
  , toAscUnfoldable
  , fromFoldable
  , fromFoldableWith
  , delete
  , pop
  , member
  , alter
  , update
  , mapWithKey
  , filterWithKey
  , filterKeys
  , filter
  , keys
  , values
  , union
  , unions
  , isSubmap
  , fold
  , foldMap
  , foldM
  , foldMaybe
  , all
  , thawST
  , freezeST
  , runST
  , pureST
  , toArrayWithKey
  ) where

import Prelude

import Control.Monad.Eff (Eff, runPure, foreachE)
import Control.Monad.ST as ST

import Data.Array as A
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldl, foldr, for_)
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.Function.Uncurried (Fn2, runFn2, Fn4, runFn4)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.StrMap.ST as SM
import Data.Traversable (class Traversable, traverse)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..), fst, uncurry)
import Data.Unfoldable (class Unfoldable)

-- | `StrMap a` represents a map from `String`s to values of type `a`.
foreign import data StrMap :: Type -> Type

foreign import _copyEff :: forall a b h r. a -> Eff (st :: ST.ST h | r) b

-- | Convert an immutable map into a mutable map
thawST :: forall a h r. StrMap a -> Eff (st :: ST.ST h | r) (SM.STStrMap h a)
thawST = _copyEff

-- | Convert a mutable map into an immutable map
freezeST :: forall a h r. SM.STStrMap h a -> Eff (st :: ST.ST h | r) (StrMap a)
freezeST = _copyEff

-- | Freeze a mutable map, creating an immutable map. Use this function as you would use
-- | `Prelude.runST` to freeze a mutable reference.
-- |
-- | The rank-2 type prevents the map from escaping the scope of `runST`.
foreign import runST :: forall a r. (forall h. Eff (st :: ST.ST h | r) (SM.STStrMap h a)) -> Eff r (StrMap a)

pureST :: forall a. (forall h e. Eff (st :: ST.ST h | e) (SM.STStrMap h a)) -> StrMap a
pureST f = runPure (runST f)

mutate :: forall a b. (forall h e. SM.STStrMap h a -> Eff (st :: ST.ST h | e) b) -> StrMap a -> StrMap a
mutate f m = pureST do
  s <- thawST m
  _ <- f s
  pure s

foreign import _fmapStrMap :: forall a b. Fn2 (StrMap a) (a -> b) (StrMap b)

instance functorStrMap :: Functor StrMap where
  map f m = runFn2 _fmapStrMap m f

instance functorWithIndexStrMap :: FunctorWithIndex String StrMap where
  mapWithIndex = mapWithKey

foreign import _foldM :: forall a m z. (m -> (z -> m) -> m) -> (z -> String -> a -> m) -> m -> StrMap a -> m

-- | Fold the keys and values of a map
fold :: forall a z. (z -> String -> a -> z) -> z -> StrMap a -> z
fold = _foldM ((#))

-- | Fold the keys and values of a map, accumulating values using
-- | some `Monoid`.
foldMap :: forall a m. Monoid m => (String -> a -> m) -> StrMap a -> m
foldMap f = fold (\acc k v -> acc <> f k v) mempty

-- | Fold the keys and values of a map, accumulating values and effects in
-- | some `Monad`.
foldM :: forall a m z. Monad m => (z -> String -> a -> m z) -> z -> StrMap a -> m z
foldM f z = _foldM bind f (pure z)

instance foldableStrMap :: Foldable StrMap where
  foldl f = fold (\z _ -> f z)
  foldr f z m = foldr f z (values m)
  foldMap f = foldMap (const f)

instance foldableWithIndexStrMap :: FoldableWithIndex String StrMap where
  foldlWithIndex f = fold (flip f)
  foldrWithIndex f z m = foldr (uncurry f) z (toArrayWithKey Tuple m)
  foldMapWithIndex = foldMap

instance traversableStrMap :: Traversable StrMap where
  traverse = traverseWithIndex <<< const
  sequence = traverse id

instance traversableWithIndexStrMap :: TraversableWithIndex String StrMap where
  traverseWithIndex f ms =
    fold (\acc k v -> flip (insert k) <$> acc <*> f k v) (pure empty) ms

-- Unfortunately the above are not short-circuitable (consider using purescript-machines)
-- so we need special cases:

foreign import _foldSCStrMap :: forall a z. Fn4 (StrMap a) z (z -> String -> a -> Maybe z) (forall b. b -> Maybe b -> b) z

-- | Fold the keys and values of a map.
-- |
-- | This function allows the folding function to terminate the fold early,
-- | using `Maybe`.
foldMaybe :: forall a z. (z -> String -> a -> Maybe z) -> z -> StrMap a -> z
foldMaybe f z m = runFn4 _foldSCStrMap m z f fromMaybe

-- | Test whether all key/value pairs in a `StrMap` satisfy a predicate.
foreign import all :: forall a. (String -> a -> Boolean) -> StrMap a -> Boolean

instance eqStrMap :: Eq a => Eq (StrMap a) where
  eq m1 m2 = (isSubmap m1 m2) && (isSubmap m2 m1)

instance eq1StrMap :: Eq1 StrMap where
  eq1 = eq

-- Internal use
toAscArray :: forall v. StrMap v -> Array (Tuple String v)
toAscArray = toAscUnfoldable

instance ordStrMap :: Ord a => Ord (StrMap a) where
  compare m1 m2 = compare (toAscArray m1) (toAscArray m2)

instance showStrMap :: Show a => Show (StrMap a) where
  show m = "(fromFoldable " <> show (toArray m) <> ")"

-- | An empty map
foreign import empty :: forall a. StrMap a

-- | Test whether one map contains all of the keys and values contained in another map
isSubmap :: forall a. Eq a => StrMap a -> StrMap a -> Boolean
isSubmap m1 m2 = all f m1 where
  f k v = runFn4 _lookup false ((==) v) k m2

-- | Test whether a map is empty
isEmpty :: forall a. StrMap a -> Boolean
isEmpty = all (\_ _ -> false)

-- | Calculate the number of key/value pairs in a map
foreign import size :: forall a. StrMap a -> Int

-- | Create a map with one key/value pair
singleton :: forall a. String -> a -> StrMap a
singleton k v = pureST do
  s <- SM.new
  SM.poke s k v

foreign import _lookup :: forall a z. Fn4 z (a -> z) String (StrMap a) z

-- | Lookup the value for a key in a map
lookup :: forall a. String -> StrMap a -> Maybe a
lookup = runFn4 _lookup Nothing Just

-- | Test whether a `String` appears as a key in a map
member :: forall a. String -> StrMap a -> Boolean
member = runFn4 _lookup false (const true)

-- | Insert or replace a key/value pair in a map
insert :: forall a. String -> a -> StrMap a -> StrMap a
insert k v = mutate (\s -> void $ SM.poke s k v)

foreign import _unsafeDeleteStrMap :: forall a. Fn2 (StrMap a) String (StrMap a)

-- | Delete a key and value from a map
delete :: forall a. String -> StrMap a -> StrMap a
delete k = mutate (\s -> void $ SM.delete s k)

-- | Delete a key and value from a map, returning the value
-- | as well as the subsequent map
pop :: forall a. String -> StrMap a -> Maybe (Tuple a (StrMap a))
pop k m = lookup k m <#> \a -> Tuple a (delete k m)

-- | Insert, remove or update a value for a key in a map
alter :: forall a. (Maybe a -> Maybe a) -> String -> StrMap a -> StrMap a
alter f k m = case f (k `lookup` m) of
  Nothing -> delete k m
  Just v -> insert k v m

-- | Remove or update a value for a key in a map
update :: forall a. (a -> Maybe a) -> String -> StrMap a -> StrMap a
update f k m = alter (maybe Nothing f) k m

-- | Create a map from a foldable collection of key/value pairs
fromFoldable :: forall f a. Foldable f => f (Tuple String a) -> StrMap a
fromFoldable l = pureST do
  s <- SM.new
  foreachE (A.fromFoldable l) \(Tuple k v) -> void (SM.poke s k v)
  pure s

foreign import _lookupST :: forall a h r z. Fn4 z (a -> z) String (SM.STStrMap h a) (Eff (st :: ST.ST h | r) z)

-- | Create a map from a foldable collection of key/value pairs, using the
-- | specified function to combine values for duplicate keys.
fromFoldableWith :: forall f a. Foldable f => (a -> a -> a) -> f (Tuple String a) -> StrMap a
fromFoldableWith f l = pureST (do
  s <- SM.new
  for_ l (\(Tuple k v) -> runFn4 _lookupST v (f v) k s >>= SM.poke s k)
  pure s)

foreign import toArrayWithKey :: forall a b . (String -> a -> b) -> StrMap a -> Array b

-- | Unfolds a map into a list of key/value pairs
toUnfoldable :: forall f a. Unfoldable f => StrMap a -> f (Tuple String a)
toUnfoldable = A.toUnfoldable <<< toArrayWithKey Tuple

-- | Unfolds a map into a list of key/value pairs which is guaranteed to be
-- | sorted by key
toAscUnfoldable :: forall f a. Unfoldable f => StrMap a -> f (Tuple String a)
toAscUnfoldable = A.toUnfoldable <<< A.sortWith fst <<< toArrayWithKey Tuple

-- Internal
toArray :: forall a. StrMap a -> Array (Tuple String a)
toArray = toArrayWithKey Tuple

-- | Get an array of the keys in a map
foreign import keys :: forall a. StrMap a -> Array String

-- | Get a list of the values in a map
values :: forall a. StrMap a -> Array a
values = toArrayWithKey (\_ v -> v)

-- | Compute the union of two maps, preferring the first map in the case of
-- | duplicate keys.
union :: forall a. StrMap a -> StrMap a -> StrMap a
union m = mutate (\s -> void $ foldM SM.poke s m)

-- | Compute the union of a collection of maps
unions :: forall f a. Foldable f => f (StrMap a) -> StrMap a
unions = foldl union empty

foreign import _mapWithKey :: forall a b. Fn2 (StrMap a) (String -> a -> b) (StrMap b)

-- | Apply a function of two arguments to each key/value pair, producing a new map
mapWithKey :: forall a b. (String -> a -> b) -> StrMap a -> StrMap b
mapWithKey f m = runFn2 _mapWithKey m f

instance semigroupStrMap :: (Semigroup a) => Semigroup (StrMap a) where
  append m1 m2 = mutate (\s1 -> void $ foldM (\s2 k v2 -> SM.poke s2 k (runFn4 _lookup v2 (\v1 -> v1 <> v2) k m2)) s1 m1) m2

instance monoidStrMap :: (Semigroup a) => Monoid (StrMap a) where
  mempty = empty

-- | Filter out those key/value pairs of a map for which a predicate
-- | fails to hold.
filterWithKey :: forall a. (String -> a -> Boolean) -> StrMap a -> StrMap a
filterWithKey predicate m = pureST go
  where
  go :: forall h e. Eff (st :: ST.ST h | e) (SM.STStrMap h a)
  go = do
    m' <- SM.new
    foldM step m' m

    where
    step acc k v = if predicate k v then SM.poke acc k v else pure acc

-- | Filter out those key/value pairs of a map for which a predicate
-- | on the key fails to hold.
filterKeys :: (String -> Boolean) -> StrMap ~> StrMap
filterKeys predicate = filterWithKey $ const <<< predicate

-- | Filter out those key/value pairs of a map for which a predicate
-- | on the value fails to hold.
filter :: forall a. (a -> Boolean) -> StrMap a -> StrMap a
filter predicate = filterWithKey $ const predicate
