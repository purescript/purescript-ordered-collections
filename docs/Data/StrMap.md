## Module Data.StrMap

This module defines a type of native Javascript maps which
require the keys to be strings.

To maximize performance, Javascript objects are not wrapped,
and some native code is used even when it's not necessary.

#### `StrMap`

``` purescript
data StrMap :: * -> *
```

`StrMap a` represents a map from `String`s to values of type `a`.

##### Instances
``` purescript
Functor StrMap
Foldable StrMap
Traversable StrMap
(Eq a) => Eq (StrMap a)
(Show a) => Show (StrMap a)
(Semigroup a) => Semigroup (StrMap a)
(Semigroup a) => Monoid (StrMap a)
```

#### `thawST`

``` purescript
thawST :: forall a h r. StrMap a -> Eff (st :: ST h | r) (STStrMap h a)
```

Convert an immutable map into a mutable map

#### `freezeST`

``` purescript
freezeST :: forall a h r. STStrMap h a -> Eff (st :: ST h | r) (StrMap a)
```

Convert a mutable map into an immutable map

#### `runST`

``` purescript
runST :: forall a r. (forall h. Eff (st :: ST h | r) (STStrMap h a)) -> Eff r (StrMap a)
```

Freeze a mutable map, creating an immutable map. Use this function as you would use
`Prelude.runST` to freeze a mutable reference.

The rank-2 type prevents the map from escaping the scope of `runST`.

#### `pureST`

``` purescript
pureST :: forall a. (forall h e. Eff (st :: ST h | e) (STStrMap h a)) -> StrMap a
```

#### `fold`

``` purescript
fold :: forall a z. (z -> String -> a -> z) -> z -> StrMap a -> z
```

Fold the keys and values of a map

#### `foldMap`

``` purescript
foldMap :: forall a m. (Monoid m) => (String -> a -> m) -> StrMap a -> m
```

Fold the keys and values of a map, accumulating values using
some `Monoid`.

#### `foldM`

``` purescript
foldM :: forall a m z. (Monad m) => (z -> String -> a -> m z) -> z -> StrMap a -> m z
```

Fold the keys and values of a map, accumulating values and effects in
some `Monad`.

#### `foldMaybe`

``` purescript
foldMaybe :: forall a z. (z -> String -> a -> Maybe z) -> z -> StrMap a -> z
```

Fold the keys and values of a map.

This function allows the folding function to terminate the fold early,
using `Maybe`.

#### `all`

``` purescript
all :: forall a. (String -> a -> Boolean) -> StrMap a -> Boolean
```

Test whether all key/value pairs in a `StrMap` satisfy a predicate.

#### `empty`

``` purescript
empty :: forall a. StrMap a
```

An empty map

#### `isSubmap`

``` purescript
isSubmap :: forall a. (Eq a) => StrMap a -> StrMap a -> Boolean
```

Test whether one map contains all of the keys and values contained in another map

#### `isEmpty`

``` purescript
isEmpty :: forall a. StrMap a -> Boolean
```

Test whether a map is empty

#### `size`

``` purescript
size :: forall a. StrMap a -> Number
```

Calculate the number of key/value pairs in a map

#### `singleton`

``` purescript
singleton :: forall a. String -> a -> StrMap a
```

Create a map with one key/value pair

#### `lookup`

``` purescript
lookup :: forall a. String -> StrMap a -> Maybe a
```

Lookup the value for a key in a map

#### `member`

``` purescript
member :: forall a. String -> StrMap a -> Boolean
```

Test whether a `String` appears as a key in a map

#### `insert`

``` purescript
insert :: forall a. String -> a -> StrMap a -> StrMap a
```

Insert a key and value into a map

#### `delete`

``` purescript
delete :: forall a. String -> StrMap a -> StrMap a
```

Delete a key and value from a map

#### `alter`

``` purescript
alter :: forall a. (Maybe a -> Maybe a) -> String -> StrMap a -> StrMap a
```

Insert, remove or update a value for a key in a map

#### `update`

``` purescript
update :: forall a. (a -> Maybe a) -> String -> StrMap a -> StrMap a
```

Remove or update a value for a key in a map

#### `fromFoldable`

``` purescript
fromFoldable :: forall f a. (Foldable f) => f (Tuple String a) -> StrMap a
```

Create a map from a foldable collection of key/value pairs

#### `fromFoldableWith`

``` purescript
fromFoldableWith :: forall f a. (Foldable f) => (a -> a -> a) -> f (Tuple String a) -> StrMap a
```

Create a map from a foldable collection of key/value pairs, using the
specified function to combine values for duplicate keys.

#### `fromList`

``` purescript
fromList :: forall a. List (Tuple String a) -> StrMap a
```

Create a map from a list of key/value pairs

#### `fromListWith`

``` purescript
fromListWith :: forall a. (a -> a -> a) -> List (Tuple String a) -> StrMap a
```

Create a map from a list of key/value pairs, using the specified function
to combine values for duplicate keys.

#### `toList`

``` purescript
toList :: forall a. StrMap a -> List (Tuple String a)
```

Convert a map into a list of key/value pairs

#### `keys`

``` purescript
keys :: forall a. StrMap a -> Array String
```

Get an array of the keys in a map

#### `values`

``` purescript
values :: forall a. StrMap a -> List a
```

Get a list of the values in a map

#### `union`

``` purescript
union :: forall a. StrMap a -> StrMap a -> StrMap a
```

Compute the union of two maps, preferring the first map in the case of
duplicate keys.

#### `unions`

``` purescript
unions :: forall a. List (StrMap a) -> StrMap a
```

Compute the union of a collection of maps


