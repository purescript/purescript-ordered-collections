--
-- Native Javascript maps which require the keys to be strings.
-- To maximize performance, Javascript objects are not wrapped,
-- and some native code is used even when it's not necessary.
--

module Data.StrMap
  ( StrMap(),
    empty,
    isEmpty,
    singleton,
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
    map,
    isSubmap,
    fold,
    foldMaybe
  ) where
  
import qualified Prelude as P

import qualified Data.Array as A
import Data.Maybe 
import Data.Function
import Data.Tuple
import Data.Foldable (foldl) 

foreign import data StrMap :: * -> *

foreign import _foldStrMap
  "function _foldStrMap(m, z0, f) {\
  \  var z = z0;\
  \  for (var k in m) {\
  \    if (m.hasOwnProperty(k)) z = f(z)(k)(m[k]);\
  \  }\
  \  return z;\
  \}" :: forall v z. Fn3 (StrMap v) z (z -> String -> v -> z) z

fold :: forall a z. (z -> String -> a -> z) -> z -> (StrMap a) -> z
fold f z m = runFn3 _foldStrMap m z f

foreign import _fmapStrMap
  "function _fmapStrMap(m0, f) {\
  \  var m = {};\
  \  for (var k in m0) {\
  \    if (m0.hasOwnProperty(k)) m[k] = f(m0[k]);\
  \  }\
  \  return m;\
  \}" :: forall a b. Fn2 (StrMap a) (a -> b) (StrMap b)

instance functorStrMap :: P.Functor StrMap where
  (<$>) f m = runFn2 _fmapStrMap m f

foreign import _foldSCStrMap
  "function _foldSCStrMap(m, z0, f, fromMaybe) { \
  \   var z = z0;                           \
  \   for (var k in m) {                    \
  \     if (m.hasOwnProperty(k)) {          \
  \       var maybeR = f(z)(k)(m[k]);       \
  \       var r = fromMaybe(null)(maybeR);  \
  \       if (r === null) return z;         \
  \       else z = r;                       \
  \     }                                   \
  \   }                                     \
  \  return z;                              \
  \}" :: forall a z. Fn4 (StrMap a) z (z -> String -> a -> Maybe z) (forall a. a -> Maybe a -> a) z

foldMaybe :: forall a z. (z -> String -> a -> Maybe z) -> z -> (StrMap a) -> z
foldMaybe f z m = runFn4 _foldSCStrMap m z f fromMaybe

instance eqStrMap :: (P.Eq a) => P.Eq (StrMap a) where
  (==) m1 m2 = (isSubmap m1 m2) P.&& (isSubmap m2 m1)
  (/=) m1 m2 = P.not (m1 P.== m2)

instance showStrMap :: (P.Show a) => P.Show (StrMap a) where
  show m = "fromList " P.++ P.show (toList m)

foreign import empty "var empty = {};" :: forall a. StrMap a

isSubmap :: forall a. (P.Eq a) => StrMap a -> StrMap a -> Boolean
isSubmap m1 m2 = foldMaybe f true m1 where
  f acc k v = if (P.not acc) then (Nothing :: Maybe Boolean) 
              else Just P.$ acc P.&& (maybe false (\v0 -> v0 P.== v) (lookup k m2))

isEmpty :: forall a. StrMap a -> Boolean
isEmpty m = size m P.== 0

foreign import size "function size(m) {\
  \  var s = 0;\
  \  for (var k in m) {\
  \    if (m.hasOwnProperty(k)) ++s;\
  \  }\
  \  return s;\
  \}" :: forall a. StrMap a -> Number

singleton :: forall a. String -> a -> StrMap a
singleton k v = insert k v empty

foreign import _lookup
  "function _lookup(m, k, yes, no) {              \
  \   if (m[k] !== undefined) return yes(m[k]);   \
  \   else return no;                             \
  \}" :: forall a z. Fn4 (StrMap a) String (a -> z) z z

lookup :: forall a. String -> StrMap a -> Maybe a
lookup k m = runFn4 _lookup m k Just Nothing

member :: forall a. String -> StrMap a -> Boolean
member k m = isJust (k `lookup` m)

foreign import _cloneStrMap
  "function _cloneStrMap(m0) { \
  \  var m = {}; \
  \  for (var k in m0) {\
  \    if (m0.hasOwnProperty(k)) m[k] = m0[k];\
  \  }\
  \  return m;\
  \}" :: forall a. (StrMap a) -> (StrMap a)

foreign import _unsafeInsertStrMap
  "function _unsafeInsertStrMap(m, k, v) {  \
  \   m[k] = v;                             \
  \   return m;                             \
  \}" :: forall a. Fn3 (StrMap a) String a (StrMap a)

insert :: forall a. String -> a -> StrMap a -> StrMap a
insert k v m = runFn3 _unsafeInsertStrMap (_cloneStrMap m) k v

foreign import _unsafeDeleteStrMap
  "function _unsafeDeleteStrMap(m, k) { \
  \   delete m[k];                      \
  \   return m;                         \
  \}" :: forall a. Fn2 (StrMap a) String (StrMap a)

delete :: forall a. String -> StrMap a -> StrMap a
delete k m = runFn2 _unsafeDeleteStrMap (_cloneStrMap m) k

alter :: forall a. (Maybe a -> Maybe a) -> String -> StrMap a -> StrMap a
alter f k m = case f (k `lookup` m) of
  Nothing -> delete k m
  Just v -> insert k v m

update :: forall a. (a -> Maybe a) -> String -> StrMap a -> StrMap a
update f k m = alter (maybe Nothing f) k m  

toList :: forall a. StrMap a -> [Tuple String a]
toList m = fold f [] m where
  f acc k v = acc P.++ [Tuple k v]

fromList :: forall a. [Tuple String a] -> StrMap a
fromList = foldl (\m (Tuple k v) -> insert k v m) empty

keys :: forall a. StrMap a -> [String]
keys m = fold f [] m where
  f acc k v = acc P.++ [k]

values :: forall a. StrMap a -> [a]
values m = fold f [] m where
  f acc k v = acc P.++ [v]

union :: forall a. StrMap a -> StrMap a -> StrMap a
union m1 m2 = foldl (\m (Tuple k v) -> insert k v m) m2 (toList m1)

unions :: forall a. [StrMap a] -> StrMap a
unions = foldl union empty

map :: forall a b. (a -> b) -> StrMap a -> StrMap b
map = P.(<$>)