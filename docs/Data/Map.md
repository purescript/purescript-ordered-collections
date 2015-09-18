## Module Data.Map

This module defines a type of maps as balanced 2-3 trees, based on
<http://www.cs.princeton.edu/~dpw/courses/cos326-12/ass/2-3-trees.pdf>

#### `Map`

``` purescript
data Map k v
```

`Map k v` represents maps from keys of type `k` to values of type `v`.

##### Instances
``` purescript
instance eqMap :: (Eq k, Eq v) => Eq (Map k v)
instance showMap :: (Show k, Show v) => Show (Map k v)
instance ordMap :: (Ord k, Ord v) => Ord (Map k v)
instance semigroupMap :: (Ord k) => Semigroup (Map k v)
instance monoidMap :: (Ord k) => Monoid (Map k v)
instance functorMap :: Functor (Map k)
instance foldableMap :: Foldable (Map k)
instance traversableMap :: (Ord k) => Traversable (Map k)
```

#### `showTree`

``` purescript
showTree :: forall k v. (Show k, Show v) => Map k v -> String
```

Render a `Map` as a `String`

#### `empty`

``` purescript
empty :: forall k v. Map k v
```

An empty map

#### `isEmpty`

``` purescript
isEmpty :: forall k v. Map k v -> Boolean
```

Test if a map is empty

#### `singleton`

``` purescript
singleton :: forall k v. k -> v -> Map k v
```

Create a map with one key/value pair

#### `checkValid`

``` purescript
checkValid :: forall k v. Map k v -> Boolean
```

Check whether the underlying tree satisfies the 2-3 invariant

This function is provided for internal use.

#### `lookup`

``` purescript
lookup :: forall k v. (Ord k) => k -> Map k v -> Maybe v
```

Lookup a value for the specified key

#### `member`

``` purescript
member :: forall k v. (Ord k) => k -> Map k v -> Boolean
```

Test if a key is a member of a map

#### `insert`

``` purescript
insert :: forall k v. (Ord k) => k -> v -> Map k v -> Map k v
```

Insert a key/value pair into a map

#### `delete`

``` purescript
delete :: forall k v. (Ord k) => k -> Map k v -> Map k v
```

Delete a key and its corresponding value from a map

#### `alter`

``` purescript
alter :: forall k v. (Ord k) => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
```

Insert the value, delete a value, or update a value for a key in a map

#### `update`

``` purescript
update :: forall k v. (Ord k) => (v -> Maybe v) -> k -> Map k v -> Map k v
```

Update or delete the value for a key in a map

#### `toList`

``` purescript
toList :: forall k v. Map k v -> List (Tuple k v)
```

Convert a map to an array of key/value pairs

#### `fromList`

``` purescript
fromList :: forall k v. (Ord k) => List (Tuple k v) -> Map k v
```

Create a map from an array of key/value pairs

#### `fromListWith`

``` purescript
fromListWith :: forall k v. (Ord k) => (v -> v -> v) -> List (Tuple k v) -> Map k v
```

Create a map from an array of key/value pairs, using the specified function
to combine values for duplicate keys.

#### `keys`

``` purescript
keys :: forall k v. Map k v -> List k
```

Get an array of the keys contained in a map

#### `values`

``` purescript
values :: forall k v. Map k v -> List v
```

Get an array of the values contained in a map

#### `unionWith`

``` purescript
unionWith :: forall k v. (Ord k) => (v -> v -> v) -> Map k v -> Map k v -> Map k v
```

Compute the union of two maps, using the specified function
to combine values for duplicate keys.

#### `union`

``` purescript
union :: forall k v. (Ord k) => Map k v -> Map k v -> Map k v
```

Compute the union of two maps, preferring values from the first map in the case
of duplicate keys

#### `unions`

``` purescript
unions :: forall k v f. (Ord k, Foldable f) => f (Map k v) -> Map k v
```

Compute the union of a collection of maps

#### `size`

``` purescript
size :: forall k v. Map k v -> Int
```

Calculate the number of key/value pairs in a map


