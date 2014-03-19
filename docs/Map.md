# Module Documentation
## Module Data.Map

### Types

    data Map k v where


### Type Classes


### Type Class Instances

    instance eqMap :: (P.Eq k, P.Eq v) => P.Eq (Map k v)

    instance showMap :: (P.Show k, P.Show v) => P.Show (Map k v)


### Values

    delete :: forall k v. (P.Eq k, P.Ord k) => k -> Map k v -> Map k v

    empty :: forall k v. Map k v

    fromList :: forall k v. (P.Eq k, P.Ord k) => [Tuple k v] -> Map k v

    insert :: forall k v. (P.Eq k, P.Ord k) => k -> v -> Map k v -> Map k v

    lookup :: forall k v. (P.Eq k, P.Ord k) => k -> Map k v -> Maybe v

    map :: forall k v1 v2. (P.Eq k, P.Ord k) => (v1 -> v2) -> Map k v1 -> Map k v2

    singleton :: forall k v. k -> v -> Map k v

    toList :: forall k v. Map k v -> [Tuple k v]

    union :: forall k v. (P.Eq k, P.Ord k) => Map k v -> Map k v -> Map k v



