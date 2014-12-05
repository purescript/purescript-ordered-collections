--
-- Maps as balanced 2-3 trees
--
-- Based on http://www.cs.princeton.edu/~dpw/courses/cos326-12/ass/2-3-trees.pdf
--

module Data.Map
  ( Map(),
    showTree,
    empty,
    isEmpty,
    singleton,
    checkValid,
    insert,
    lookup,
    toList,
    fromList,
    delete,
    member,
    alter,
    update,
    keys,
    values,
    union,
    unions,
    map
  ) where

import qualified Prelude as P

import qualified Data.Array as A
import Data.Maybe
import Data.Tuple
import Data.Foldable (foldl, foldMap, foldr, Foldable)
import Data.Traversable (traverse, Traversable)

data Map k v
  = Leaf
  | Two (Map k v) k v (Map k v)
  | Three (Map k v) k v (Map k v) k v (Map k v)

instance eqMap :: (P.Eq k, P.Eq v) => P.Eq (Map k v) where
  (==) m1 m2 = toList m1 P.== toList m2
  (/=) m1 m2 = P.not (m1 P.== m2)

instance showMap :: (P.Show k, P.Show v) => P.Show (Map k v) where
  show m = "fromList " P.++ P.show (toList m)

instance functorMap :: P.Functor (Map k) where
  (<$>) _ Leaf = Leaf
  (<$>) f (Two left k v right) = Two (f P.<$> left) k (f v) (f P.<$> right)
  (<$>) f (Three left k1 v1 mid k2 v2 right) = Three (f P.<$> left) k1 (f v1) (f P.<$> mid) k2 (f v2) (f P.<$> right)

instance foldableMap :: Foldable (Map k) where
  foldl   f z m = foldl   f z (values m)
  foldr   f z m = foldr   f z (values m)
  foldMap f   m = foldMap f   (values m)

instance traversableMap :: (P.Ord k) => Traversable (Map k) where
  traverse f ms = foldr (\x acc -> union P.<$> x P.<*> acc) (P.pure empty) ((P.(<$>) (uncurry singleton)) P.<$> (traverse f P.<$> toList ms))
  sequence = traverse P.id

showTree :: forall k v. (P.Show k, P.Show v) => Map k v -> String
showTree Leaf = "Leaf"
showTree (Two left k v right) =
  "Two (" P.++ showTree left P.++
  ") (" P.++ P.show k P.++
  ") (" P.++ P.show v P.++
  ") (" P.++ showTree right P.++ ")"
showTree (Three left k1 v1 mid k2 v2 right) =
  "Three (" P.++ showTree left P.++
  ") (" P.++ P.show k1 P.++
  ") (" P.++ P.show v1 P.++
  ") (" P.++ showTree mid P.++
  ") (" P.++ P.show k2 P.++
  ") (" P.++ P.show v2 P.++
  ") (" P.++ showTree right P.++ ")"

empty :: forall k v. Map k v
empty = Leaf

isEmpty :: forall k v. Map k v -> Boolean
isEmpty Leaf = true
isEmpty _ = false

singleton :: forall k v. k -> v -> Map k v
singleton k v = Two Leaf k v Leaf

checkValid :: forall k v. Map k v -> Boolean
checkValid tree = A.length (A.nub (allHeights tree)) P.== 1
  where
  allHeights :: forall k v. Map k v -> [Number]
  allHeights Leaf = [0]
  allHeights (Two left _ _ right) = A.map (\n -> n P.+ 1) (allHeights left P.++ allHeights right)
  allHeights (Three left _ _ mid _ _ right) = A.map (\n -> n P.+ 1) (allHeights left P.++ allHeights mid P.++ allHeights right)

lookup :: forall k v. (P.Ord k) => k -> Map k v -> Maybe v
lookup _ Leaf = Nothing
lookup k (Two _ k1 v _) | k P.== k1 = Just v
lookup k (Two left k1 _ _) | k P.< k1 = lookup k left
lookup k (Two _ _ _ right) = lookup k right
lookup k (Three _ k1 v1 _ _ _ _) | k P.== k1 = Just v1
lookup k (Three _ _ _ _ k2 v2 _) | k P.== k2 = Just v2
lookup k (Three left k1 _ _ _ _ _) | k P.< k1 = lookup k left
lookup k (Three _ k1 _ mid k2 _ _) | k1 P.< k P.&& k P.<= k2 = lookup k mid
lookup k (Three _ _ _ _ _ _ right) = lookup k right

member :: forall k v. (P.Ord k) => k -> Map k v -> Boolean
member k m = isJust (k `lookup` m)

data TreeContext k v
  = TwoLeft k v (Map k v)
  | TwoRight (Map k v) k v
  | ThreeLeft k v (Map k v) k v (Map k v)
  | ThreeMiddle (Map k v) k v k v (Map k v)
  | ThreeRight (Map k v) k v (Map k v) k v

fromZipper :: forall k v. (P.Ord k) => [TreeContext k v] -> Map k v -> Map k v
fromZipper [] tree = tree
fromZipper (TwoLeft k1 v1 right : ctx) left = fromZipper ctx (Two left k1 v1 right)
fromZipper (TwoRight left k1 v1 : ctx) right = fromZipper ctx (Two left k1 v1 right)
fromZipper (ThreeLeft k1 v1 mid k2 v2 right : ctx) left = fromZipper ctx (Three left k1 v1 mid k2 v2 right)
fromZipper (ThreeMiddle left k1 v1 k2 v2 right : ctx) mid = fromZipper ctx (Three left k1 v1 mid k2 v2 right)
fromZipper (ThreeRight left k1 v1 mid k2 v2 : ctx) right = fromZipper ctx (Three left k1 v1 mid k2 v2 right)

data KickUp k v = KickUp (Map k v) k v (Map k v)

insert :: forall k v. (P.Ord k) => k -> v -> Map k v -> Map k v
insert = down []
  where
  down :: forall k v. (P.Ord k) => [TreeContext k v] -> k -> v -> Map k v -> Map k v
  down ctx k v Leaf = up ctx (KickUp Leaf k v Leaf)
  down ctx k v (Two left k1 _ right) | k P.== k1 = fromZipper ctx (Two left k v right)
  down ctx k v (Two left k1 v1 right) | k P.< k1 = down (TwoLeft k1 v1 right P.: ctx) k v left
  down ctx k v (Two left k1 v1 right) = down (TwoRight left k1 v1 P.: ctx) k v right
  down ctx k v (Three left k1 _ mid k2 v2 right) | k P.== k1 = fromZipper ctx (Three left k v mid k2 v2 right)
  down ctx k v (Three left k1 v1 mid k2 _ right) | k P.== k2 = fromZipper ctx (Three left k1 v1 mid k v right)
  down ctx k v (Three left k1 v1 mid k2 v2 right) | k P.< k1 = down (ThreeLeft k1 v1 mid k2 v2 right P.: ctx) k v left
  down ctx k v (Three left k1 v1 mid k2 v2 right) | k1 P.< k P.&& k P.<= k2 = down (ThreeMiddle left k1 v1 k2 v2 right P.: ctx) k v mid
  down ctx k v (Three left k1 v1 mid k2 v2 right) = down (ThreeRight left k1 v1 mid k2 v2 P.: ctx) k v right

  up :: forall k v. (P.Ord k) => [TreeContext k v] -> KickUp k v -> Map k v
  up [] (KickUp left k v right) = Two left k v right
  up (TwoLeft k1 v1 right : ctx) (KickUp left k v mid) = fromZipper ctx (Three left k v mid k1 v1 right)
  up (TwoRight left k1 v1 : ctx) (KickUp mid k v right) = fromZipper ctx (Three left k1 v1 mid k v right)
  up (ThreeLeft k1 v1 c k2 v2 d : ctx) (KickUp a k v b) = up ctx (KickUp (Two a k v b) k1 v1 (Two c k2 v2 d))
  up (ThreeMiddle a k1 v1 k2 v2 d : ctx) (KickUp b k v c) = up ctx (KickUp (Two a k1 v1 b) k v (Two c k2 v2 d))
  up (ThreeRight a k1 v1 b k2 v2 : ctx) (KickUp c k v d) = up ctx (KickUp (Two a k1 v1 b) k2 v2 (Two c k v d))

delete :: forall k v. (P.Ord k) => k -> Map k v -> Map k v
delete = down []
  where
  down :: forall k v. (P.Ord k) => [TreeContext k v] -> k -> Map k v -> Map k v
  down ctx _ Leaf = fromZipper ctx Leaf
  down ctx k (Two Leaf k1 _ Leaf) 
    | k P.== k1 = up ctx Leaf
  down ctx k (Two left k1 v1 right) 
    | k P.== k1   = let max = maxNode left
                    in removeMaxNode (TwoLeft max.key max.value right P.: ctx) left
    | k P.< k1    = down (TwoLeft k1 v1 right P.: ctx) k left
    | P.otherwise = down (TwoRight left k1 v1 P.: ctx) k right
  down ctx k (Three Leaf k1 v1 Leaf k2 v2 Leaf) 
    | k P.== k1 = fromZipper ctx (Two Leaf k2 v2 Leaf)
    | k P.== k2 = fromZipper ctx (Two Leaf k1 v1 Leaf)
  down ctx k (Three left k1 v1 mid k2 v2 right) 
    | k P.== k1 = let max = maxNode left
                  in removeMaxNode (ThreeLeft max.key max.value mid k2 v2 right P.: ctx) left
    | k P.== k2 = let max = maxNode mid
                  in removeMaxNode (ThreeMiddle left k1 v1 max.key max.value right P.: ctx) mid
    | k P.< k1               = down (ThreeLeft k1 v1 mid k2 v2 right P.: ctx) k left
    | k1 P.< k P.&& k P.< k2 = down (ThreeMiddle left k1 v1 k2 v2 right P.: ctx) k mid
    | P.otherwise            = down (ThreeRight left k1 v1 mid k2 v2 P.: ctx) k right

  up :: forall k v. (P.Ord k) => [TreeContext k v] -> Map k v -> Map k v
  up [] tree = tree
  up (TwoLeft k1 v1 Leaf : ctx) Leaf = fromZipper ctx (Two Leaf k1 v1 Leaf)
  up (TwoRight Leaf k1 v1 : ctx) Leaf = fromZipper ctx (Two Leaf k1 v1 Leaf)
  up (TwoLeft k1 v1 (Two m k2 v2 r) : ctx) l = up ctx (Three l k1 v1 m k2 v2 r)
  up (TwoRight (Two l k1 v1 m) k2 v2 : ctx) r = up ctx (Three l k1 v1 m k2 v2 r)
  up (TwoLeft k1 v1 (Three b k2 v2 c k3 v3 d) : ctx) a = fromZipper ctx (Two (Two a k1 v1 b) k2 v2 (Two c k3 v3 d))
  up (TwoRight (Three a k1 v1 b k2 v2 c) k3 v3 : ctx) d = fromZipper ctx (Two (Two a k1 v1 b) k2 v2 (Two c k3 v3 d))
  up (ThreeLeft k1 v1 Leaf k2 v2 Leaf : ctx) Leaf = fromZipper ctx (Three Leaf k1 v1 Leaf k2 v2 Leaf)
  up (ThreeMiddle Leaf k1 v1 k2 v2 Leaf : ctx) Leaf = fromZipper ctx (Three Leaf k1 v1 Leaf k2 v2 Leaf)
  up (ThreeRight Leaf k1 v1 Leaf k2 v2 : ctx) Leaf = fromZipper ctx (Three Leaf k1 v1 Leaf k2 v2 Leaf)
  up (ThreeLeft k1 v1 (Two b k2 v2 c) k3 v3 d : ctx) a = fromZipper ctx (Two (Three a k1 v1 b k2 v2 c) k3 v3 d)
  up (ThreeMiddle (Two a k1 v1 b) k2 v2 k3 v3 d : ctx) c = fromZipper ctx (Two (Three a k1 v1 b k2 v2 c) k3 v3 d)
  up (ThreeMiddle a k1 v1 k2 v2 (Two c k3 v3 d) : ctx) b = fromZipper ctx (Two a k1 v1 (Three b k2 v2 c k3 v3 d))
  up (ThreeRight a k1 v1 (Two b k2 v2 c) k3 v3 : ctx) d = fromZipper ctx (Two a k1 v1 (Three b k2 v2 c k3 v3 d))
  up (ThreeLeft k1 v1 (Three b k2 v2 c k3 v3 d) k4 v4 e : ctx) a = fromZipper ctx (Three (Two a k1 v1 b) k2 v2 (Two c k3 v3 d) k4 v4 e)
  up (ThreeMiddle (Three a k1 v1 b k2 v2 c) k3 v3 k4 v4 e : ctx) d = fromZipper ctx (Three (Two a k1 v1 b) k2 v2 (Two c k3 v3 d) k4 v4 e)
  up (ThreeMiddle a k1 v1 k2 v2 (Three c k3 v3 d k4 v4 e) : ctx) b = fromZipper ctx (Three a k1 v1 (Two b k2 v2 c) k3 v3 (Two d k4 v4 e))
  up (ThreeRight a k1 v1 (Three b k2 v2 c k3 v3 d) k4 v4 : ctx) e = fromZipper ctx (Three a k1 v1 (Two b k2 v2 c) k3 v3 (Two d k4 v4 e))

  maxNode :: forall k v. (P.Ord k) => Map k v -> { key :: k, value :: v }
  maxNode (Two _ k v Leaf) = { key: k, value: v }
  maxNode (Two _ _ _ right) = maxNode right
  maxNode (Three _ _ _ _ k v Leaf) = { key: k, value: v }
  maxNode (Three _ _ _ _ _ _ right) = maxNode right

  removeMaxNode :: forall k v. (P.Ord k) => [TreeContext k v] -> Map k v -> Map k v
  removeMaxNode ctx (Two Leaf _ _ Leaf) = up ctx Leaf
  removeMaxNode ctx (Two left k v right) = removeMaxNode (TwoRight left k v P.: ctx) right
  removeMaxNode ctx (Three Leaf k1 v1 Leaf _ _ Leaf) = up (TwoRight Leaf k1 v1 P.: ctx) Leaf
  removeMaxNode ctx (Three left k1 v1 mid k2 v2 right) = removeMaxNode (ThreeRight left k1 v1 mid k2 v2 P.: ctx) right

alter :: forall k v. (P.Ord k) => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
alter f k m = case f (k `lookup` m) of
  Nothing -> delete k m
  Just v -> insert k v m

update :: forall k v. (P.Ord k) => (v -> Maybe v) -> k -> Map k v -> Map k v
update f k m = alter (maybe Nothing f) k m

toList :: forall k v. Map k v -> [Tuple k v]
toList Leaf = []
toList (Two left k v right) = toList left P.++ [Tuple k v] P.++ toList right
toList (Three left k1 v1 mid k2 v2 right) = toList left P.++ [Tuple k1 v1] P.++ toList mid P.++ [Tuple k2 v2] P.++ toList right

fromList :: forall k v. (P.Ord k) => [Tuple k v] -> Map k v
fromList = foldl (\m (Tuple k v) -> insert k v m) empty

keys :: forall k v. Map k v -> [k]
keys Leaf = []
keys (Two left k _ right) = keys left P.++ [k] P.++ keys right
keys (Three left k1 _ mid k2 _ right) = keys left P.++ [k1] P.++ keys mid P.++ [k2] P.++ keys right

values :: forall k v. Map k v -> [v]
values Leaf = []
values (Two left _ v right) = values left P.++ [v] P.++ values right
values (Three left _ v1 mid _ v2 right) = values left P.++ [v1] P.++ values mid P.++ [v2] P.++ values right

union :: forall k v. (P.Ord k) => Map k v -> Map k v -> Map k v
union m1 m2 = foldl (\m (Tuple k v) -> insert k v m) m2 (toList m1)

unions :: forall k v. (P.Ord k) => [Map k v] -> Map k v
unions = foldl union empty

map :: forall k a b. (a -> b) -> Map k a -> Map k b
map = P.(<$>)

size :: forall k v. Map k v -> Number
size = A.length P.<<< values
