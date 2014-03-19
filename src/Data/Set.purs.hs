module Data.Set
  ( Set(),
    empty,
    singleton,
    insert,
    member,
    delete,
    toList,
    fromList,
    union
  ) where

import qualified Prelude as P

import Data.Array (concat)
import Data.Foldable (foldl)
import Data.Maybe
import Data.Tuple

data Set a = Leaf | Branch { value :: a, left :: Set a, right :: Set a }

instance eqSet :: (P.Eq a) => P.Eq (Set a) where
  (==) s1 s2 = toList s1 P.== toList s2
  (/=) s1 s2 = P.not (s1 P.== s2)

instance showSet :: (P.Show a) => P.Show (Set a) where
  show s = "fromList " P.++ P.show (toList s)

empty :: forall a. Set a
empty = Leaf

singleton :: forall a. a -> Set a
singleton a = Branch { value: a, left: empty, right: empty }

insert :: forall a. (P.Eq a, P.Ord a) => a -> Set a -> Set a
insert a Leaf = singleton a
insert a (Branch b@{ value = a1 }) | a P.== a1 = Branch b
insert a (Branch b@{ value = a1 }) | a P.< a1 = Branch (b { left = insert a b.left })
insert a (Branch b) = Branch (b { right = insert a b.right })

member :: forall a. (P.Eq a, P.Ord a) => a -> Set a -> Boolean
member _ Leaf = false
member a (Branch { value = a1 }) | a P.== a1 = true
member a (Branch { value = a1, left = left }) | a P.< a1 = member a left
member a (Branch { right = right }) = member a right

findMinValue :: forall a. (P.Ord a) => Set a -> a
findMinValue (Branch { value = a, left = Leaf }) = a
findMinValue (Branch b) = findMinValue b.left

delete :: forall a. (P.Eq a, P.Ord a) => a -> Set a -> Set a
delete _ Leaf = Leaf
delete a (Branch b@{ value = a1, left = Leaf }) | a P.== a1 =
  case b of
    { left = Leaf } -> b.right
    { right = Leaf } -> b.left
    _ -> let minValue = findMinValue b.right in
         Branch (b { value = minValue, right = delete minValue b.right })
delete a (Branch b@{ value = a1 }) | a P.< a1 = Branch (b { left = delete a b.left })
delete a (Branch b) = Branch (b { right = delete a b.right })

toList :: forall a. Set a -> [a]
toList Leaf = []
toList (Branch b) = toList b.left `concat` [b.value] `concat` toList b.right

fromList :: forall a. (P.Eq a, P.Ord a) => [a] -> Set a
fromList = foldl (\s a -> insert a s) empty

union :: forall a. (P.Eq a, P.Ord a) => Set a -> Set a -> Set a
union s1 s2 = foldl (\s a -> insert a s) s2 (toList s1)

