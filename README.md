# Module Documentation

## Module Data.Graph

### Types

    data Edge v where

    data Graph v where


### Values

    scc :: forall v. (Eq v, Ord v) => Graph v -> [[v]]

    topSort :: forall v. (Eq v, Ord v) => Graph v -> [v]


## Module Data.Map

### Types

    data Map k v where


### Type Class Instances

    instance eqMap :: (P.Eq k, P.Eq v) => P.Eq (Map k v)

    instance showMap :: (P.Show k, P.Show v) => P.Show (Map k v)


### Values

    alter :: forall k v. (P.Eq k, P.Ord k) => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v

    delete :: forall k v. (P.Eq k, P.Ord k) => k -> Map k v -> Map k v

    empty :: forall k v. Map k v

    fromList :: forall k v. (P.Eq k, P.Ord k) => [Tuple k v] -> Map k v

    insert :: forall k v. (P.Eq k, P.Ord k) => k -> v -> Map k v -> Map k v

    lookup :: forall k v. (P.Eq k, P.Ord k) => k -> Map k v -> Maybe v

    map :: forall k v1 v2. (P.Eq k, P.Ord k) => (v1 -> v2) -> Map k v1 -> Map k v2

    singleton :: forall k v. k -> v -> Map k v

    toList :: forall k v. Map k v -> [Tuple k v]

    union :: forall k v. (P.Eq k, P.Ord k) => Map k v -> Map k v -> Map k v


## Module Data.Set

### Types

    data Set a where


### Type Class Instances

    instance eqSet :: (P.Eq a) => P.Eq (Set a)

    instance showSet :: (P.Show a) => P.Show (Set a)


### Values

    delete :: forall a. (P.Eq a, P.Ord a) => a -> Set a -> Set a

    empty :: forall a. Set a

    fromList :: forall a. (P.Eq a, P.Ord a) => [a] -> Set a

    insert :: forall a. (P.Eq a, P.Ord a) => a -> Set a -> Set a

    member :: forall a. (P.Eq a, P.Ord a) => a -> Set a -> Prim.Boolean

    singleton :: forall a. a -> Set a

    toList :: forall a. Set a -> [a]

    union :: forall a. (P.Eq a, P.Ord a) => Set a -> Set a -> Set a