# Module Documentation

## Module Data.Map

### Types

    data Map k v


### Type Class Instances

    instance eqMap :: (P.Eq k, P.Eq v) => P.Eq (Map k v)

    instance foldableMap :: Foldable (Map k)

    instance functorMap :: P.Functor (Map k)

    instance showMap :: (P.Show k, P.Show v) => P.Show (Map k v)

    instance traversableMap :: (P.Ord k) => Traversable (Map k)


### Values

    alter :: forall k v. (P.Ord k) => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v

    checkValid :: forall k v. Map k v -> Boolean

    delete :: forall k v. (P.Ord k) => k -> Map k v -> Map k v

    empty :: forall k v. Map k v

    fromList :: forall k v. (P.Ord k) => [Tuple k v] -> Map k v

    insert :: forall k v. (P.Ord k) => k -> v -> Map k v -> Map k v

    isEmpty :: forall k v. Map k v -> Boolean

    keys :: forall k v. Map k v -> [k]

    lookup :: forall k v. (P.Ord k) => k -> Map k v -> Maybe v

    map :: forall k a b. (a -> b) -> Map k a -> Map k b

    member :: forall k v. (P.Ord k) => k -> Map k v -> Boolean

    showTree :: forall k v. (P.Show k, P.Show v) => Map k v -> String

    singleton :: forall k v. k -> v -> Map k v

    size :: forall k v. Map k v -> Number

    toList :: forall k v. Map k v -> [Tuple k v]

    union :: forall k v. (P.Ord k) => Map k v -> Map k v -> Map k v

    unions :: forall k v. (P.Ord k) => [Map k v] -> Map k v

    update :: forall k v. (P.Ord k) => (v -> Maybe v) -> k -> Map k v -> Map k v

    values :: forall k v. Map k v -> [v]


## Module Data.StrMap

### Types

    data StrMap :: * -> *


### Type Class Instances

    instance eqStrMap :: (P.Eq a) => P.Eq (StrMap a)

    instance foldableStrMap :: Foldable StrMap

    instance functorStrMap :: P.Functor StrMap

    instance semigroupStrMap :: (P.Semigroup a) => P.Semigroup (StrMap a)

    instance showStrMap :: (P.Show a) => P.Show (StrMap a)

    instance traversableStrMap :: Traversable StrMap


### Values

    all :: forall a. (String -> a -> Boolean) -> StrMap a -> Boolean

    alter :: forall a. (Maybe a -> Maybe a) -> String -> StrMap a -> StrMap a

    delete :: forall a. String -> StrMap a -> StrMap a

    empty :: forall a. StrMap a

    fold :: forall a z. (z -> String -> a -> z) -> z -> StrMap a -> z

    foldM :: forall a m z. (P.Monad m) => (z -> String -> a -> m z) -> z -> StrMap a -> m z

    foldMap :: forall a m. (Monoid m) => (String -> a -> m) -> StrMap a -> m

    foldMaybe :: forall a z. (z -> String -> a -> Maybe z) -> z -> StrMap a -> z

    freezeST :: forall a h r. SM.STStrMap h a -> Eff (st :: ST.ST h | r) (StrMap a)

    fromList :: forall a. [Tuple String a] -> StrMap a

    insert :: forall a. String -> a -> StrMap a -> StrMap a

    isEmpty :: forall a. StrMap a -> Boolean

    isSubmap :: forall a. (P.Eq a) => StrMap a -> StrMap a -> Boolean

    keys :: forall a. StrMap a -> [String]

    lookup :: forall a. String -> StrMap a -> Maybe a

    map :: forall a b. (a -> b) -> StrMap a -> StrMap b

    member :: forall a. String -> StrMap a -> Boolean

    runST :: forall a r. (forall h. Eff (st :: ST.ST h | r) (SM.STStrMap h a)) -> Eff r (StrMap a)

    singleton :: forall a. String -> a -> StrMap a

    size :: forall a. StrMap a -> Number

    thawST :: forall a h r. StrMap a -> Eff (st :: ST.ST h | r) (SM.STStrMap h a)

    toList :: forall a. StrMap a -> [Tuple String a]

    union :: forall a. StrMap a -> StrMap a -> StrMap a

    unions :: forall a. [StrMap a] -> StrMap a

    update :: forall a. (a -> Maybe a) -> String -> StrMap a -> StrMap a

    values :: forall a. StrMap a -> [a]


## Module Data.StrMap.ST

### Types

    data STStrMap :: * -> * -> *


### Values

    delete :: forall a h r. STStrMap h a -> String -> Eff (st :: ST h | r) (STStrMap h a)

    new :: forall a h r. Eff (st :: ST h | r) (STStrMap h a)

    peek :: forall a h r. STStrMap h a -> String -> Eff (st :: ST h | r) a

    poke :: forall a h r. STStrMap h a -> String -> a -> Eff (st :: ST h | r) (STStrMap h a)


## Module Data.StrMap.ST.Unsafe

### Values

    unsafeGet :: forall a h r. STStrMap h a -> Eff (st :: ST h | r) (StrMap a)


## Module Data.StrMap.Unsafe

### Values

    unsafeIndex :: forall a. StrMap a -> String -> a