# Module Documentation
## Module Data.Set

### Types

    data Set a where


### Type Classes


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



