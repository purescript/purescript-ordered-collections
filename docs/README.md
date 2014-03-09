# Module Documentation
## Module Data.Map

### Types

    data Map k v where


### Type Classes


### Type Class Instances

    instance eqMapI :: (Eq k, Eq v) => Eq (Map k v)

    instance showMapI :: (Show k, Show v) => Show (Map k v)


### Values

    delete :: forall k v. (Eq k, Ord k) => k -> Map k v -> Map k v

    empty :: forall k v. Map k v

    eqMap :: forall k v. (Eq k, Eq v) => Map k v -> Map k v -> Prim.Boolean

    fromList :: forall k v. (Eq k, Ord k) => [Tuple k v] -> Map k v

    insert :: forall k v. (Eq k, Ord k) => k -> v -> Map k v -> Map k v

    lookup :: forall k v. (Eq k, Ord k) => k -> Map k v -> Maybe v

    showMap :: forall k v. (Show k, Show v) => Map k v -> Prim.String

    singleton :: forall k v. k -> v -> Map k v

    toList :: forall k v. Map k v -> [Tuple k v]

    union :: forall k v. (Eq k, Ord k) => Map k v -> Map k v -> Map k v



