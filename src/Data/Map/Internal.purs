-- | This module defines a type of maps as height-balanced (AVL) binary trees.
-- | Efficient set operations are implemented in terms of
-- | <https://www.cs.cmu.edu/~guyb/papers/BFS16.pdf>

module Data.Map.Internal
  ( Map(..)
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
  , any
  , anyWithKey
  , MapIter
  , MapIterStep(..)
  , toMapIter
  , stepAsc
  , stepAscCps
  , stepDesc
  , stepDescCps
  , stepUnordered
  , stepUnorderedCps
  , unsafeNode
  , unsafeBalancedNode
  , unsafeJoinNodes
  , unsafeSplit
  , Split(..)
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Plus (class Plus)
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldl, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex, foldlWithIndex, foldrWithIndex)
import Data.Function.Uncurried (Fn2, Fn3, Fn4, Fn7, mkFn2, mkFn3, mkFn4, mkFn7, runFn2, runFn3, runFn4, runFn7)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Ord (class Ord1, abs)
import Data.Traversable (traverse, class Traversable)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Data.Tuple (Tuple(Tuple))
import Data.Unfoldable (class Unfoldable, unfoldr)
import Prim.TypeError (class Warn, Text)

-- | `Map k v` represents maps from keys of type `k` to values of type `v`.
data Map k v = Leaf | Node Int Int k v (Map k v) (Map k v)

type role Map nominal representational

instance eq1Map :: Eq k => Eq1 (Map k) where
  eq1 = eq

instance eqMap :: (Eq k, Eq v) => Eq (Map k v) where
  eq xs ys = case xs of
    Leaf ->
      case ys of
        Leaf -> true
        _ -> false
    Node _ s1 _ _ _ _ ->
      case ys of
        Node _ s2 _ _ _ _
          | s1 == s2 ->
              toMapIter xs == toMapIter ys
        _ ->
          false

instance ord1Map :: Ord k => Ord1 (Map k) where
  compare1 = compare

instance ordMap :: (Ord k, Ord v) => Ord (Map k v) where
  compare xs ys = case xs of
    Leaf ->
      case ys of
        Leaf -> EQ
        _ -> LT
    _ ->
      case ys of
        Leaf -> GT
        _ -> compare (toMapIter xs) (toMapIter ys)

instance showMap :: (Show k, Show v) => Show (Map k v) where
  show as = "(fromFoldable " <> show (toUnfoldable as :: Array _) <> ")"

instance semigroupMap ::
  ( Warn (Text "Data.Map's `Semigroup` instance is now unbiased and differs from the left-biased instance defined in PureScript releases <= 0.13.x.")
  , Ord k
  , Semigroup v
  ) => Semigroup (Map k v) where
  append = unionWith append

instance monoidSemigroupMap ::
  ( Warn (Text "Data.Map's `Semigroup` instance is now unbiased and differs from the left-biased instance defined in PureScript releases <= 0.13.x.")
  , Ord k
  , Semigroup v
  ) => Monoid (Map k v) where
  mempty = empty

instance altMap :: Ord k => Alt (Map k) where
  alt = union

instance plusMap :: Ord k => Plus (Map k) where
  empty = empty

instance functorMap :: Functor (Map k) where
  map f = go
    where
    go = case _ of
      Leaf -> Leaf
      Node h s k v l r ->
        Node h s k (f v) (go l) (go r)

instance functorWithIndexMap :: FunctorWithIndex k (Map k) where
  mapWithIndex f = go
    where
    go = case _ of
      Leaf -> Leaf
      Node h s k v l r ->
        Node h s k (f k v) (go l) (go r)

instance applyMap :: Ord k => Apply (Map k) where
  apply = intersectionWith identity

instance bindMap :: Ord k => Bind (Map k) where
  bind m f = mapMaybeWithKey (\k -> lookup k <<< f) m

instance foldableMap :: Foldable (Map k) where
  foldr f z = \m -> runFn2 go m z
    where
    go = mkFn2 \m' z' -> case m' of
      Leaf -> z'
      Node _ _ _ v l r ->
        runFn2 go l (f v (runFn2 go r z'))
  foldl f z = \m -> runFn2 go z m
    where
    go = mkFn2 \z' m' -> case m' of
      Leaf -> z'
      Node _ _ _ v l r ->
        runFn2 go (f (runFn2 go z' l) v) r
  foldMap f = go
    where
    go = case _ of
      Leaf -> mempty
      Node _ _ _ v l r ->
        go l <> f v <> go r

instance foldableWithIndexMap :: FoldableWithIndex k (Map k) where
  foldrWithIndex f z = \m -> runFn2 go m z
    where
    go = mkFn2 \m' z' -> case m' of
      Leaf -> z'
      Node _ _ k v l r ->
        runFn2 go l (f k v (runFn2 go r z'))
  foldlWithIndex f z = \m -> runFn2 go z m
    where
    go = mkFn2 \z' m' -> case m' of
      Leaf -> z'
      Node _ _ k v l r ->
        runFn2 go (f k (runFn2 go z' l) v) r
  foldMapWithIndex f = go
    where
    go = case _ of
      Leaf -> mempty
      Node _ _ k v l r ->
        go l <> f k v <> go r

instance traversableMap :: Traversable (Map k) where
  traverse f = go
    where
    go = case _ of
      Leaf -> pure Leaf
      Node h s k v l r ->
        (\l' v' r' -> Node h s k v' l' r')
          <$> go l
          <*> f v
          <*> go r
  sequence = traverse identity

instance traversableWithIndexMap :: TraversableWithIndex k (Map k) where
  traverseWithIndex f = go
    where
    go = case _ of
      Leaf -> pure Leaf
      Node h s k v l r ->
        (\l' v' r' -> Node h s k v' l' r')
          <$> go l
          <*> f k v
          <*> go r

-- | Render a `Map` as a `String`
showTree :: forall k v. Show k => Show v => Map k v -> String
showTree = go ""
  where
  go ind = case _ of
    Leaf -> ind <> "Leaf"
    Node h _ k v l r ->
      (ind <> "[" <> show h  <> "] " <> show k <> " => " <> show v <> "\n")
        <> (go (ind <> "    ") l <> "\n")
        <> (go (ind <> "    ") r)

-- | An empty map
empty :: forall k v. Map k v
empty = Leaf

-- | Test if a map is empty
isEmpty :: forall k v. Map k v -> Boolean
isEmpty Leaf = true
isEmpty _ = false

-- | Create a map with one key/value pair
singleton :: forall k v. k -> v -> Map k v
singleton k v = Node 1 1 k v Leaf Leaf

-- | Check whether the underlying tree satisfies the height, size, and ordering invariants.
-- |
-- | This function is provided for internal use.
checkValid :: forall k v. Ord k => Map k v -> Boolean
checkValid = go
  where
  go = case _ of
    Leaf -> true
    Node h s k _ l r ->
      case l of
        Leaf ->
          case r of
            Leaf ->
              true
            Node rh rs rk _ _ _ ->
              h == 2 && rh == 1 && s > rs && rk > k && go r
        Node lh ls lk _ _ _ ->
          case r of
            Leaf ->
              h == 2 && lh == 1 && s > ls && lk < k && go l
            Node rh rs rk _ _ _ ->
              h > rh && rk > k && h > lh && lk < k && abs (rh - lh) < 2 && rs + ls + 1 == s && go l && go r

-- | Look up a value for the specified key
lookup :: forall k v. Ord k => k -> Map k v -> Maybe v
lookup k = go
  where
  go = case _ of
    Leaf -> Nothing
    Node _ _ mk mv ml mr ->
      case compare k mk of
        LT -> go ml
        GT -> go mr
        EQ -> Just mv

-- | Look up a value for the specified key, or the greatest one less than it
lookupLE :: forall k v. Ord k => k -> Map k v -> Maybe { key :: k, value :: v }
lookupLE k = go
  where
  go = case _ of
    Leaf -> Nothing
    Node _ _ mk mv ml mr ->
      case compare k mk of
        LT -> go ml
        GT ->
          case go mr of
            Nothing -> Just { key: mk, value: mv }
            other -> other
        EQ ->
          Just { key: mk, value: mv }

-- | Look up a value for the greatest key less than the specified key
lookupLT :: forall k v. Ord k => k -> Map k v -> Maybe { key :: k, value :: v }
lookupLT k = go
  where
  go = case _ of
    Leaf -> Nothing
    Node _ _ mk mv ml mr ->
      case compare k mk of
        LT -> go ml
        GT ->
          case go mr of
            Nothing -> Just { key: mk, value: mv }
            other -> other
        EQ ->
          findMax ml

-- | Look up a value for the specified key, or the least one greater than it
lookupGE :: forall k v. Ord k => k -> Map k v -> Maybe { key :: k, value :: v }
lookupGE k = go
  where
  go = case _ of
    Leaf -> Nothing
    Node _ _ mk mv ml mr ->
      case compare k mk of
        LT ->
          case go ml of
            Nothing -> Just { key: mk, value: mv }
            other -> other
        GT -> go mr
        EQ -> Just { key: mk, value: mv }

-- | Look up a value for the least key greater than the specified key
lookupGT :: forall k v. Ord k => k -> Map k v -> Maybe { key :: k, value :: v }
lookupGT k = go
  where
  go = case _ of
    Leaf -> Nothing
    Node _ _ mk mv ml mr ->
      case compare k mk of
        LT ->
          case go ml of
            Nothing -> Just { key: mk, value: mv }
            other -> other
        GT -> go mr
        EQ -> findMin mr

-- | Returns the pair with the greatest key
findMax :: forall k v. Map k v -> Maybe { key :: k, value :: v }
findMax = case _ of
  Leaf -> Nothing
  Node _ _ k v  _ r ->
    case r of
      Leaf -> Just { key: k, value: v }
      _ -> findMax r

-- | Returns the pair with the least key
findMin :: forall k v. Map k v -> Maybe { key :: k, value :: v }
findMin = case _ of
  Leaf -> Nothing
  Node _ _ k v l _ ->
    case l of
      Leaf -> Just { key: k, value: v }
      _ -> findMin l

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
foldSubmap = foldSubmapBy (<>) mempty

foldSubmapBy :: forall k v m. Ord k => (m -> m -> m) -> m -> Maybe k -> Maybe k -> (k -> v -> m) -> Map k v -> m
foldSubmapBy appendFn memptyValue kmin kmax f =
  let
    tooSmall =
      case kmin of
        Just kmin' ->
          \k -> k < kmin'
        Nothing ->
          const false

    tooLarge =
      case kmax of
        Just kmax' ->
          \k -> k > kmax'
        Nothing ->
          const false

    inBounds =
      case kmin, kmax of
        Just kmin', Just kmax' ->
          \k -> kmin' <= k && k <= kmax'
        Just kmin', Nothing ->
          \k -> kmin' <= k
        Nothing, Just kmax' ->
          \k -> k <= kmax'
        Nothing, Nothing ->
          const true

    go = case _ of
      Leaf ->
        memptyValue
      Node _ _ k v left right ->
                    (if tooSmall k then memptyValue else go left)
        `appendFn` (if inBounds k then f k v else memptyValue)
        `appendFn` (if tooLarge k then memptyValue else go right)
  in
    go

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
submap kmin kmax = foldSubmapBy union empty kmin kmax singleton

-- | Test if a key is a member of a map
member :: forall k v. Ord k => k -> Map k v -> Boolean
member k = go
  where
  go = case _ of
    Leaf -> false
    Node _ _ mk _ ml mr ->
      case compare k mk of
        LT -> go ml
        GT -> go mr
        EQ -> true

-- | Insert or replace a key/value pair in a map
insert :: forall k v. Ord k => k -> v -> Map k v -> Map k v
insert k v = go
  where
  go = case _ of
    Leaf -> singleton k v
    Node mh ms mk mv ml mr ->
      case compare k mk of
        LT -> runFn4 unsafeBalancedNode mk mv (go ml) mr
        GT -> runFn4 unsafeBalancedNode mk mv ml (go mr)
        EQ -> Node mh ms k v ml mr

-- | Inserts or updates a value with the given function.
-- |
-- | The combining function is called with the existing value as the first
-- | argument and the new value as the second argument.
insertWith :: forall k v. Ord k => (v -> v -> v) -> k -> v -> Map k v -> Map k v
insertWith app k v = go
  where
  go = case _ of
    Leaf -> singleton k v
    Node mh ms mk mv ml mr ->
      case compare k mk of
        LT -> runFn4 unsafeBalancedNode mk mv (go ml) mr
        GT -> runFn4 unsafeBalancedNode mk mv ml (go mr)
        EQ -> Node mh ms k (app mv v) ml mr

-- | Delete a key and its corresponding value from a map.
delete :: forall k v. Ord k => k -> Map k v -> Map k v
delete k = go
  where
  go = case _ of
    Leaf -> Leaf
    Node _ _ mk mv ml mr ->
      case compare k mk of
        LT -> runFn4 unsafeBalancedNode mk mv (go ml) mr
        GT -> runFn4 unsafeBalancedNode mk mv ml (go mr)
        EQ -> runFn2 unsafeJoinNodes ml mr

-- | Delete a key and its corresponding value from a map, returning the value
-- | as well as the subsequent map.
pop :: forall k v. Ord k => k -> Map k v -> Maybe (Tuple v (Map k v))
pop k m = do
  let (Split x l r) = runFn3 unsafeSplit compare k m
  map (\a -> Tuple a (runFn2 unsafeJoinNodes l r)) x

-- | Insert the value, delete a value, or update a value for a key in a map
alter :: forall k v. Ord k => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
alter f k m = do
  let Split v l r = runFn3 unsafeSplit compare k m
  case f v of
    Nothing ->
      runFn2 unsafeJoinNodes l r
    Just v' ->
      runFn4 unsafeBalancedNode k v' l r

-- | Update or delete the value for a key in a map
update :: forall k v. Ord k => (v -> Maybe v) -> k -> Map k v -> Map k v
update f k = go
  where
  go = case _ of
    Leaf -> Leaf
    Node mh ms mk mv ml mr ->
      case compare k mk of
        LT -> runFn4 unsafeBalancedNode mk mv (go ml) mr
        GT -> runFn4 unsafeBalancedNode mk mv ml (go mr)
        EQ ->
          case f mv of
            Nothing ->
              runFn2 unsafeJoinNodes ml mr
            Just mv' ->
              Node mh ms mk mv' ml mr

-- | Convert any foldable collection of key/value pairs to a map.
-- | On key collision, later values take precedence over earlier ones.
fromFoldable :: forall f k v. Ord k => Foldable f => f (Tuple k v) -> Map k v
fromFoldable = foldl (\m (Tuple k v) -> insert k v m) empty

-- | Convert any foldable collection of key/value pairs to a map.
-- | On key collision, the values are configurably combined.
fromFoldableWith :: forall f k v. Ord k => Foldable f => (v -> v -> v) -> f (Tuple k v) -> Map k v
fromFoldableWith f = foldl (\m (Tuple k v) -> f' k v m) empty
  where
  f' = insertWith (flip f)

-- | Convert any indexed foldable collection into a map.
fromFoldableWithIndex :: forall f k v. Ord k => FoldableWithIndex k f => f v -> Map k v
fromFoldableWithIndex = foldlWithIndex (\k m v -> insert k v m) empty

-- | Convert a map to an unfoldable structure of key/value pairs where the keys are in ascending order
toUnfoldable :: forall f k v. Unfoldable f => Map k v -> f (Tuple k v)
toUnfoldable = unfoldr stepUnfoldr <<< toMapIter

-- | Convert a map to an unfoldable structure of key/value pairs
-- |
-- | While this traversal is up to 10% faster in benchmarks than `toUnfoldable`,
-- | it leaks the underlying map stucture, making it only suitable for applications
-- | where order is irrelevant.
-- |
-- | If you are unsure, use `toUnfoldable`
toUnfoldableUnordered :: forall f k v. Unfoldable f => Map k v -> f (Tuple k v)
toUnfoldableUnordered = unfoldr stepUnfoldrUnordered <<< toMapIter

-- | Get a list of the keys contained in a map
keys :: forall k v. Map k v -> List k
keys = foldrWithIndex (\k _ acc -> k : acc) Nil

-- | Get a list of the values contained in a map
values :: forall k v. Map k v -> List v
values = foldr Cons Nil

-- | Compute the union of two maps, using the specified function
-- | to combine values for duplicate keys.
unionWith :: forall k v. Ord k => (v -> v -> v) -> Map k v -> Map k v -> Map k v
unionWith app m1 m2 = runFn4 unsafeUnionWith compare app m1 m2

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
intersectionWith app m1 m2 = runFn4 unsafeIntersectionWith compare app m1 m2

-- | Compute the intersection of two maps, preferring values from the first map in the case
-- | of duplicate keys.
intersection :: forall k a b. Ord k => Map k a -> Map k b -> Map k a
intersection = intersectionWith const

-- | Difference of two maps. Return elements of the first map where
-- | the keys do not exist in the second map.
difference :: forall k v w. Ord k => Map k v -> Map k w -> Map k v
difference m1 m2 = runFn3 unsafeDifference compare m1 m2

-- | Test whether one map contains all of the keys and values contained in another map
isSubmap :: forall k v. Ord k => Eq v => Map k v -> Map k v -> Boolean
isSubmap = go
  where
  go m1 m2 = case m1 of
    Leaf -> true
    Node _ _ k v l r ->
      case lookup k m2 of
        Nothing -> false
        Just v' ->
          v == v' && go l m2 && go r m2

-- | Calculate the number of key/value pairs in a map
size :: forall k v. Map k v -> Int
size = case _ of
  Leaf -> 0
  Node _ s _ _ _ _ -> s

-- | Filter out those key/value pairs of a map for which a predicate
-- | fails to hold.
filterWithKey :: forall k v. Ord k => (k -> v -> Boolean) -> Map k v -> Map k v
filterWithKey f = go
  where
  go = case _ of
    Leaf -> Leaf
    Node _ _ k v l r
      | f k v ->
          runFn4 unsafeBalancedNode k v (go l) (go r)
      | otherwise ->
          runFn2 unsafeJoinNodes (go l) (go r)

-- | Filter out those key/value pairs of a map for which a predicate
-- | on the key fails to hold.
filterKeys :: forall k. Ord k => (k -> Boolean) -> Map k ~> Map k
filterKeys f = go
  where
  go = case _ of
    Leaf -> Leaf
    Node _ _ k v l r
      | f k ->
          runFn4 unsafeBalancedNode k v (go l) (go r)
      | otherwise ->
          runFn2 unsafeJoinNodes (go l) (go r)

-- | Filter out those key/value pairs of a map for which a predicate
-- | on the value fails to hold.
filter :: forall k v. Ord k => (v -> Boolean) -> Map k v -> Map k v
filter = filterWithKey <<< const

-- | Applies a function to each key/value pair in a map, discarding entries
-- | where the function returns `Nothing`.
mapMaybeWithKey :: forall k a b. Ord k => (k -> a -> Maybe b) -> Map k a -> Map k b
mapMaybeWithKey f = go
  where
  go = case _ of
    Leaf -> Leaf
    Node _ _ k v l r ->
      case f k v of
        Just v' ->
          runFn4 unsafeBalancedNode k v' (go l) (go r)
        Nothing ->
          runFn2 unsafeJoinNodes (go l) (go r)

-- | Applies a function to each value in a map, discarding entries where the
-- | function returns `Nothing`.
mapMaybe :: forall k a b. Ord k => (a -> Maybe b) -> Map k a -> Map k b
mapMaybe = mapMaybeWithKey <<< const

-- | Filter a map of optional values, keeping only the key/value pairs which
-- | contain a value, creating a new map.
catMaybes :: forall k v. Ord k => Map k (Maybe v) -> Map k v
catMaybes = mapMaybe identity

-- | Returns true if at least one map element satisfies the given predicateon the value,
-- | iterating the map only as necessary and stopping as soon as the predicate
-- | yields true.
any :: forall k v. (v -> Boolean) -> Map k v -> Boolean
any predicate = go
  where
  go = case _ of
    Leaf                -> false
    Node _ _ _ mv ml mr -> predicate mv || go ml || go mr

-- | Returns true if at least one map element satisfies the given predicate,
-- | iterating the map only as necessary and stopping as soon as the predicate
-- | yields true.
anyWithKey :: forall k v. (k -> v -> Boolean) -> Map k v -> Boolean
anyWithKey predicate = go
  where
  go = case _ of
    Leaf                -> false
    Node _ _ mk mv ml mr -> predicate mk mv || go ml || go mr

-- | Low-level Node constructor which maintains the height and size invariants
-- | This is unsafe because it assumes the child Maps are ordered and balanced.
unsafeNode :: forall k v. Fn4 k v (Map k v) (Map k v) (Map k v)
unsafeNode = mkFn4 \k v l r -> case l of
  Leaf ->
    case r of
      Leaf ->
        Node 1 1 k v l r
      Node h2 s2 _ _ _ _ ->
        Node (1 + h2) (1 + s2) k v l r
  Node h1 s1 _ _ _ _ ->
    case r of
      Leaf ->
        Node (1 + h1) (1 + s1) k v l r
      Node h2 s2 _ _ _ _ ->
        Node (1 + if h1 > h2 then h1 else h2) (1 + s1 + s2) k v l r

-- | Low-level Node constructor which maintains the balance invariants.
-- | This is unsafe because it assumes the child Maps are ordered.
unsafeBalancedNode :: forall k v. Fn4 k v (Map k v) (Map k v) (Map k v)
unsafeBalancedNode = mkFn4 \k v l r -> case l of
  Leaf ->
    case r of
      Leaf ->
        singleton k v
      Node rh _ rk rv rl rr
        | rh > 1 ->
            runFn7 rotateLeft k v l rk rv rl rr
      _ ->
        runFn4 unsafeNode k v l r
  Node lh _ lk lv ll lr ->
    case r of
      Node rh _ rk rv rl rr
        | rh > lh + 1 ->
            runFn7 rotateLeft k v l rk rv rl rr
        | lh > rh + 1 ->
            runFn7 rotateRight k v lk lv ll lr r
      Leaf
        | lh > 1 ->
            runFn7 rotateRight k v lk lv ll lr r
      _ ->
        runFn4 unsafeNode k v l r
  where
  rotateLeft :: Fn7 k v (Map k v) k v (Map k v) (Map k v) (Map k v)
  rotateLeft = mkFn7 \k v l rk rv rl rr -> case rl of
    Node lh _ lk lv ll lr
      | lh > height rr ->
          runFn4 unsafeNode lk lv (runFn4 unsafeNode k v l ll) (runFn4 unsafeNode rk rv lr rr)
    _ ->
      runFn4 unsafeNode rk rv (runFn4 unsafeNode k v l rl) rr

  rotateRight :: Fn7 k v k v (Map k v) (Map k v) (Map k v) (Map k v)
  rotateRight = mkFn7 \k v lk lv ll lr r -> case lr of
    Node rh _ rk rv rl rr
      | height ll <= rh ->
          runFn4 unsafeNode rk rv (runFn4 unsafeNode lk lv ll rl) (runFn4 unsafeNode k v rr r)
    _ ->
      runFn4 unsafeNode lk lv ll (runFn4 unsafeNode k v lr r)

  height :: Map k v -> Int
  height = case _ of
    Leaf -> 0
    Node h _ _ _ _ _ -> h

-- | Low-level Node constructor from two Maps.
-- | This is unsafe because it assumes the child Maps are ordered.
unsafeJoinNodes :: forall k v. Fn2 (Map k v) (Map k v) (Map k v)
unsafeJoinNodes = mkFn2 case _, _ of
  Leaf, b -> b
  Node _ _ lk lv ll lr, r -> do
    let (SplitLast k v l) = runFn4 unsafeSplitLast lk lv ll lr
    runFn4 unsafeBalancedNode k v l r

data SplitLast k v = SplitLast k v (Map k v)

-- | Reassociates a node by moving the last node to the top.
-- | This is unsafe because it assumes the key and child Maps are from
-- | a balanced node.
unsafeSplitLast :: forall k v. Fn4 k v (Map k v) (Map k v) (SplitLast k v)
unsafeSplitLast = mkFn4 \k v l r -> case r of
  Leaf -> SplitLast k v l
  Node _ _ rk rv rl rr -> do
    let (SplitLast k' v' t') = runFn4 unsafeSplitLast rk rv rl rr
    SplitLast k' v' (runFn4 unsafeBalancedNode k v l t')

data Split k v = Split (Maybe v) (Map k v) (Map k v)

-- | Reassocates a Map so the given key is at the top.
-- | This is unsafe because it assumes the ordering function is appropriate.
unsafeSplit :: forall k v. Fn3 (k -> k -> Ordering) k (Map k v) (Split k v)
unsafeSplit = mkFn3 \comp k m -> case m of
  Leaf ->
    Split Nothing Leaf Leaf
  Node _ _ mk mv ml mr ->
    case comp k mk of
      LT -> do
        let (Split b ll lr) = runFn3 unsafeSplit comp k ml
        Split b ll (runFn4 unsafeBalancedNode mk mv lr mr)
      GT -> do
        let (Split b rl rr) = runFn3 unsafeSplit comp k mr
        Split b (runFn4 unsafeBalancedNode mk mv ml rl) rr
      EQ ->
        Split (Just mv) ml mr

-- | Low-level unionWith implementation.
-- | This is unsafe because it assumes the ordering function is appropriate.
unsafeUnionWith :: forall k v. Fn4 (k -> k -> Ordering) (v -> v -> v) (Map k v) (Map k v) (Map k v)
unsafeUnionWith = mkFn4 \comp app l r -> case l, r of
  Leaf, _ -> r
  _, Leaf -> l
  _, Node _ _ rk rv rl rr -> do
    let (Split lv ll lr) = runFn3 unsafeSplit comp rk l
    let l' = runFn4 unsafeUnionWith comp app ll rl
    let r' = runFn4 unsafeUnionWith comp app lr rr
    case lv of
      Just lv' ->
        runFn4 unsafeBalancedNode rk (app lv' rv) l' r'
      Nothing ->
        runFn4 unsafeBalancedNode rk rv l' r'

-- | Low-level intersectionWith implementation.
-- | This is unsafe because it assumes the ordering function is appropriate.
unsafeIntersectionWith :: forall k a b c. Fn4 (k -> k -> Ordering) (a -> b -> c) (Map k a) (Map k b) (Map k c)
unsafeIntersectionWith = mkFn4 \comp app l r -> case l, r of
  Leaf, _ -> Leaf
  _, Leaf -> Leaf
  _, Node _ _ rk rv rl rr -> do
    let (Split lv ll lr) = runFn3 unsafeSplit comp rk l
    let l' = runFn4 unsafeIntersectionWith comp app ll rl
    let r' = runFn4 unsafeIntersectionWith comp app lr rr
    case lv of
      Just lv' ->
        runFn4 unsafeBalancedNode rk (app lv' rv) l' r'
      Nothing ->
        runFn2 unsafeJoinNodes l' r'

-- | Low-level difference implementation.
-- | This is unsafe because it assumes the ordering function is appropriate.
unsafeDifference :: forall k v w. Fn3 (k -> k -> Ordering) (Map k v) (Map k w) (Map k v)
unsafeDifference = mkFn3 \comp l r -> case l, r of
  Leaf, _ -> Leaf
  _, Leaf -> l
  _, Node _ _ rk _ rl rr -> do
    let (Split _ ll lr) = runFn3 unsafeSplit comp rk l
    let l' = runFn3 unsafeDifference comp ll rl
    let r' = runFn3 unsafeDifference comp lr rr
    runFn2 unsafeJoinNodes l' r'

data MapIterStep k v
  = IterDone
  | IterNext k v (MapIter k v)

-- | Low-level iteration state for a `Map`. Must be consumed using
-- | an appropriate stepper.
data MapIter k v
  = IterLeaf
  | IterEmit k v (MapIter k v)
  | IterNode (Map k v) (MapIter k v)

instance (Eq k, Eq v) => Eq (MapIter k v) where
  eq = go
    where
    go a b = case stepAsc a of
      IterNext k1 v1 a' ->
        case stepAsc b of
          IterNext k2 v2 b'
            | k1 == k2 && v1 == v2 ->
                go a' b'
          _ ->
            false
      IterDone ->
        true

instance (Ord k, Ord v) => Ord (MapIter k v) where
  compare = go
    where
    go a b = case stepAsc a, stepAsc b of
      IterNext k1 v1 a', IterNext k2 v2 b' ->
        case compare k1 k2 of
          EQ ->
            case compare v1 v2 of
              EQ ->
                go a' b'
              other ->
                other
          other ->
            other
      IterDone, b'->
        case b' of
          IterDone ->
            EQ
          _ ->
            LT
      _, IterDone ->
        GT

-- | Converts a Map to a MapIter for iteration using a MapStepper.
toMapIter :: forall k v. Map k v -> MapIter k v
toMapIter = flip IterNode IterLeaf

type MapStepper k v = MapIter k v -> MapIterStep k v

type MapStepperCps k v = forall r. (Fn3 k v (MapIter k v) r) -> (Unit -> r) -> MapIter k v -> r

-- | Steps a `MapIter` in ascending order.
stepAsc :: forall k v. MapStepper k v
stepAsc = stepAscCps (mkFn3 \k v next -> IterNext k v next) (const IterDone)

-- | Steps a `MapIter` in descending order.
stepDesc :: forall k v. MapStepper k v
stepDesc = stepDescCps (mkFn3 \k v next -> IterNext k v next) (const IterDone)

-- | Steps a `MapIter` in arbitrary order.
stepUnordered :: forall k v. MapStepper k v
stepUnordered = stepUnorderedCps (mkFn3 \k v next -> IterNext k v next) (const IterDone)

-- | Steps a `MapIter` in ascending order with a CPS encoding.
stepAscCps :: forall k v. MapStepperCps k v
stepAscCps = stepWith iterMapL

-- | Steps a `MapIter` in descending order with a CPS encoding.
stepDescCps :: forall k v. MapStepperCps k v
stepDescCps = stepWith iterMapR

-- | Steps a `MapIter` in arbitrary order with a CPS encoding.
stepUnorderedCps :: forall k v. MapStepperCps k v
stepUnorderedCps = stepWith iterMapU

stepUnfoldr :: forall k v. MapIter k v -> Maybe (Tuple (Tuple k v) (MapIter k v))
stepUnfoldr = stepAscCps step (\_ -> Nothing)
  where
  step = mkFn3 \k v next ->
    Just (Tuple (Tuple k v) next)

stepUnfoldrUnordered :: forall k v. MapIter k v -> Maybe (Tuple (Tuple k v) (MapIter k v))
stepUnfoldrUnordered = stepUnorderedCps step (\_ -> Nothing)
  where
  step = mkFn3 \k v next ->
    Just (Tuple (Tuple k v) next)

stepWith :: forall k v r. (MapIter k v -> Map k v -> MapIter k v) -> (Fn3 k v (MapIter k v) r) -> (Unit -> r) -> MapIter k v -> r
stepWith f next done = go
  where
  go = case _ of
    IterLeaf ->
      done unit
    IterEmit k v iter ->
      runFn3 next k v iter
    IterNode m iter ->
      go (f iter m)

iterMapL :: forall k v. MapIter k v -> Map k v -> MapIter k v
iterMapL = go
  where
  go iter = case _ of
    Leaf -> iter
    Node _ _ k v l r ->
      case r of
        Leaf ->
          go (IterEmit k v iter) l
        _ ->
          go (IterEmit k v (IterNode r iter)) l

iterMapR :: forall k v. MapIter k v -> Map k v -> MapIter k v
iterMapR = go
  where
  go iter = case _ of
    Leaf -> iter
    Node _ _ k v l r ->
      case r of
        Leaf ->
          go (IterEmit k v iter) l
        _ ->
          go (IterEmit k v (IterNode l iter)) r

iterMapU :: forall k v. MapIter k v -> Map k v -> MapIter k v
iterMapU iter = case _ of
  Leaf -> iter
  Node _ _ k v l r ->
    case l of
      Leaf ->
        case r of
          Leaf ->
            IterEmit k v iter
          _ ->
            IterEmit k v (IterNode r iter)
      _ ->
        case r of
          Leaf ->
            IterEmit k v (IterNode l iter)
          _ ->
            IterEmit k v (IterNode l (IterNode r iter))
