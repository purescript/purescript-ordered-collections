# Module Documentation

## Module Data.Map

#### `Map`

``` purescript
data Map k v
```


#### `eqMap`

``` purescript
instance eqMap :: (P.Eq k, P.Eq v) => P.Eq (Map k v)
```


#### `showMap`

``` purescript
instance showMap :: (P.Show k, P.Show v) => P.Show (Map k v)
```


#### `semigroupMap`

``` purescript
instance semigroupMap :: (P.Ord k) => P.Semigroup (Map k v)
```


#### `monoidMap`

``` purescript
instance monoidMap :: (P.Ord k) => Monoid (Map k v)
```


#### `functorMap`

``` purescript
instance functorMap :: P.Functor (Map k)
```


#### `foldableMap`

``` purescript
instance foldableMap :: Foldable (Map k)
```


#### `traversableMap`

``` purescript
instance traversableMap :: (P.Ord k) => Traversable (Map k)
```


#### `showTree`

``` purescript
showTree :: forall k v. (P.Show k, P.Show v) => Map k v -> String
```


#### `empty`

``` purescript
empty :: forall k v. Map k v
```


#### `isEmpty`

``` purescript
isEmpty :: forall k v. Map k v -> Boolean
```


#### `singleton`

``` purescript
singleton :: forall k v. k -> v -> Map k v
```


#### `checkValid`

``` purescript
checkValid :: forall k v. Map k v -> Boolean
```


#### `lookup`

``` purescript
lookup :: forall k v. (P.Ord k) => k -> Map k v -> Maybe v
```


#### `member`

``` purescript
member :: forall k v. (P.Ord k) => k -> Map k v -> Boolean
```


#### `insert`

``` purescript
insert :: forall k v. (P.Ord k) => k -> v -> Map k v -> Map k v
```


#### `delete`

``` purescript
delete :: forall k v. (P.Ord k) => k -> Map k v -> Map k v
```


#### `alter`

``` purescript
alter :: forall k v. (P.Ord k) => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
```


#### `update`

``` purescript
update :: forall k v. (P.Ord k) => (v -> Maybe v) -> k -> Map k v -> Map k v
```


#### `toList`

``` purescript
toList :: forall k v. Map k v -> [Tuple k v]
```


#### `fromList`

``` purescript
fromList :: forall k v. (P.Ord k) => [Tuple k v] -> Map k v
```


#### `keys`

``` purescript
keys :: forall k v. Map k v -> [k]
```


#### `values`

``` purescript
values :: forall k v. Map k v -> [v]
```


#### `unionWith`

``` purescript
unionWith :: forall k v. (P.Ord k) => (v -> v -> v) -> Map k v -> Map k v -> Map k v
```

#### `union`

``` purescript
union :: forall k v. (P.Ord k) => Map k v -> Map k v -> Map k v
```


#### `unions`

``` purescript
unions :: forall k v. (P.Ord k) => [Map k v] -> Map k v
```


#### `map`

``` purescript
map :: forall k a b. (a -> b) -> Map k a -> Map k b
```


#### `size`

``` purescript
size :: forall k v. Map k v -> Number
```



## Module Data.StrMap

#### `StrMap`

``` purescript
data StrMap :: * -> *
```


#### `thawST`

``` purescript
thawST :: forall a h r. StrMap a -> Eff (st :: ST.ST h | r) (SM.STStrMap h a)
```


#### `freezeST`

``` purescript
freezeST :: forall a h r. SM.STStrMap h a -> Eff (st :: ST.ST h | r) (StrMap a)
```


#### `runST`

``` purescript
runST :: forall a r. (forall h. Eff (st :: ST.ST h | r) (SM.STStrMap h a)) -> Eff r (StrMap a)
```


#### `functorStrMap`

``` purescript
instance functorStrMap :: P.Functor StrMap
```


#### `fold`

``` purescript
fold :: forall a z. (z -> String -> a -> z) -> z -> StrMap a -> z
```


#### `foldMap`

``` purescript
foldMap :: forall a m. (Monoid m) => (String -> a -> m) -> StrMap a -> m
```


#### `foldM`

``` purescript
foldM :: forall a m z. (P.Monad m) => (z -> String -> a -> m z) -> z -> StrMap a -> m z
```


#### `foldableStrMap`

``` purescript
instance foldableStrMap :: Foldable StrMap
```


#### `traversableStrMap`

``` purescript
instance traversableStrMap :: Traversable StrMap
```


#### `foldMaybe`

``` purescript
foldMaybe :: forall a z. (z -> String -> a -> Maybe z) -> z -> StrMap a -> z
```


#### `all`

``` purescript
all :: forall a. (String -> a -> Boolean) -> StrMap a -> Boolean
```


#### `eqStrMap`

``` purescript
instance eqStrMap :: (P.Eq a) => P.Eq (StrMap a)
```


#### `showStrMap`

``` purescript
instance showStrMap :: (P.Show a) => P.Show (StrMap a)
```


#### `empty`

``` purescript
empty :: forall a. StrMap a
```


#### `isSubmap`

``` purescript
isSubmap :: forall a. (P.Eq a) => StrMap a -> StrMap a -> Boolean
```


#### `isEmpty`

``` purescript
isEmpty :: forall a. StrMap a -> Boolean
```


#### `size`

``` purescript
size :: forall a. StrMap a -> Number
```


#### `singleton`

``` purescript
singleton :: forall a. String -> a -> StrMap a
```


#### `lookup`

``` purescript
lookup :: forall a. String -> StrMap a -> Maybe a
```


#### `member`

``` purescript
member :: forall a. String -> StrMap a -> Boolean
```


#### `insert`

``` purescript
insert :: forall a. String -> a -> StrMap a -> StrMap a
```


#### `delete`

``` purescript
delete :: forall a. String -> StrMap a -> StrMap a
```


#### `alter`

``` purescript
alter :: forall a. (Maybe a -> Maybe a) -> String -> StrMap a -> StrMap a
```


#### `update`

``` purescript
update :: forall a. (a -> Maybe a) -> String -> StrMap a -> StrMap a
```


#### `fromList`

``` purescript
fromList :: forall a. [Tuple String a] -> StrMap a
```


#### `toList`

``` purescript
toList :: forall a. StrMap a -> [Tuple String a]
```


#### `keys`

``` purescript
keys :: forall a. StrMap a -> [String]
```


#### `values`

``` purescript
values :: forall a. StrMap a -> [a]
```


#### `union`

``` purescript
union :: forall a. StrMap a -> StrMap a -> StrMap a
```

#### `unions`

``` purescript
unions :: forall a. [StrMap a] -> StrMap a
```


#### `map`

``` purescript
map :: forall a b. (a -> b) -> StrMap a -> StrMap b
```


#### `semigroupStrMap`

``` purescript
instance semigroupStrMap :: (P.Semigroup a) => P.Semigroup (StrMap a)
```



## Module Data.StrMap.ST

#### `STStrMap`

``` purescript
data STStrMap :: * -> * -> *
```


#### `new`

``` purescript
new :: forall a h r. Eff (st :: ST h | r) (STStrMap h a)
```


#### `peek`

``` purescript
peek :: forall a h r. STStrMap h a -> String -> Eff (st :: ST h | r) a
```


#### `poke`

``` purescript
poke :: forall a h r. STStrMap h a -> String -> a -> Eff (st :: ST h | r) (STStrMap h a)
```


#### `delete`

``` purescript
delete :: forall a h r. STStrMap h a -> String -> Eff (st :: ST h | r) (STStrMap h a)
```



## Module Data.StrMap.ST.Unsafe

#### `unsafeGet`

``` purescript
unsafeGet :: forall a h r. STStrMap h a -> Eff (st :: ST h | r) (StrMap a)
```



## Module Data.StrMap.Unsafe

#### `unsafeIndex`

``` purescript
unsafeIndex :: forall a. StrMap a -> String -> a
```