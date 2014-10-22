--
-- Native Javascript maps which require the keys to be strings.
-- To maximize performance, Javascript objects are not wrapped,
-- and some native code is used even when it's not necessary.
--

module Data.StrMap
  ( StrMap(),
    empty,
    isEmpty,
    size,
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
    foldMap,
    foldM,
    foldMaybe,
    all,

    thawST,
    freezeST,
    runST
  ) where

import qualified Prelude as P

import Control.Monad.Eff (Eff(), runPure)
import Data.Foldable (Foldable, foldl, foldr, for_)
import Data.Function
import Data.Maybe
import Data.Monoid
import Data.Monoid.All
import Data.Tuple
import Data.Traversable (Traversable, traverse)
import qualified Control.Monad.ST as ST
import qualified Data.Array as A
import qualified Data.StrMap.ST as SM

foreign import data StrMap :: * -> *

foreign import _copy """
  function _copy(m) {
    var r = {};
    for (var k in m)
      r[k] = m[k]
    return r;
  }""" :: forall a. StrMap a -> StrMap a

foreign import _copyEff """
  function _copyEff(m) {
    return function () {
      return _copy(m);
    };
  }""" :: forall a b h r. a -> Eff (st :: ST.ST h | r) b

thawST :: forall a h r. StrMap a -> Eff (st :: ST.ST h | r) (SM.STStrMap h a)
thawST = _copyEff

freezeST :: forall a h r. SM.STStrMap h a -> Eff (st :: ST.ST h | r) (StrMap a)
freezeST = _copyEff

foreign import runST """
  function runST(f) {
    return f;
  }""" :: forall a r. (forall h. Eff (st :: ST.ST h | r) (SM.STStrMap h a)) -> Eff r (StrMap a)

pureST :: forall a b. (forall h e. Eff (st :: ST.ST h | e) (SM.STStrMap h a)) -> StrMap a
pureST f = runPure (runST f)

mutate :: forall a b. (forall h e. SM.STStrMap h a -> Eff (st :: ST.ST h | e) b) -> StrMap a -> StrMap a
mutate f m = pureST (do
  s <- thawST m
  f s
  P.return s)

foreign import _fmapStrMap
  "function _fmapStrMap(m0, f) {\
  \  var m = {};\
  \  for (var k in m0) {\
  \    m[k] = f(m0[k]);\
  \  }\
  \  return m;\
  \}" :: forall a b. Fn2 (StrMap a) (a -> b) (StrMap b)

instance functorStrMap :: P.Functor StrMap where
  (<$>) f m = runFn2 _fmapStrMap m f

foreign import _foldM
  "function _foldM(bind) {\
  \  return function(f) {\
  \    return function (mz) {\
  \      return function (m) {\
  \        var k;\
  \        function g(z) {\
  \          return f(z)(k)(m[k]);\
  \        }\
  \        for (k in m)\
  \          mz = bind(mz)(g);\
  \        return mz;\
  \      };\
  \    };\
  \  };\
  \}" :: forall a m z. (m -> (z -> m) -> m) -> (z -> String -> a -> m) -> m -> StrMap a -> m

fold :: forall a z. (z -> String -> a -> z) -> z -> StrMap a -> z
fold = _foldM (P.(#))

foldMap :: forall a m. (Monoid m) => (String -> a -> m) -> StrMap a -> m
foldMap f = fold (\acc k v -> acc P.<> f k v) mempty

foldM :: forall a m z. (P.Monad m) => (z -> String -> a -> m z) -> z -> StrMap a -> m z
foldM f z = _foldM P.(>>=) f (P.pure z)

instance foldableStrMap :: Foldable StrMap where
  foldl f = fold (\z _ -> f z)
  foldr f z m = foldr f z (values m)
  foldMap f = foldMap (P.const f)

instance traversableStrMap :: Traversable StrMap where
  traverse f ms = foldr (\x acc -> union P.<$> x P.<*> acc) (P.pure empty) ((P.(<$>) (uncurry singleton)) P.<$> (traverse f P.<$> toList ms))
  sequence = traverse P.id

-- Unfortunately the above are not short-circuitable (consider using purescript-machines)
-- so we need special cases:

foreign import _foldSCStrMap
  "function _foldSCStrMap(m, z, f, fromMaybe) { \
  \   for (var k in m) {                    \
  \     var maybeR = f(z)(k)(m[k]);       \
  \     var r = fromMaybe(null)(maybeR);  \
  \     if (r === null) return z;         \
  \     else z = r;                       \
  \   }                                     \
  \  return z;                              \
  \}" :: forall a z. Fn4 (StrMap a) z (z -> String -> a -> Maybe z) (forall a. a -> Maybe a -> a) z

foldMaybe :: forall a z. (z -> String -> a -> Maybe z) -> z -> StrMap a -> z
foldMaybe f z m = runFn4 _foldSCStrMap m z f fromMaybe

foreign import all
  "function all(f) {\
  \  return function (m) {\
  \    for (var k in m)\
  \      if (!f(k)(m[k])) return false;\
  \    return true;\
  \  };\
  \}" :: forall a. (String -> a -> Boolean) -> StrMap a -> Boolean

instance eqStrMap :: (P.Eq a) => P.Eq (StrMap a) where
  (==) m1 m2 = (isSubmap m1 m2) P.&& (isSubmap m2 m1)
  (/=) m1 m2 = P.not (m1 P.== m2)

instance showStrMap :: (P.Show a) => P.Show (StrMap a) where
  show m = "fromList " P.++ P.show (toList m)

foreign import empty "var empty = {};" :: forall a. StrMap a

isSubmap :: forall a. (P.Eq a) => StrMap a -> StrMap a -> Boolean
isSubmap m1 m2 = all f m1 where
  f k v = runFn4 _lookup false (P.(==) v) k m2

isEmpty :: forall a. StrMap a -> Boolean
isEmpty = all (\_ _ -> false)

foreign import size "function size(m) {\
  \  var s = 0;\
  \  for (var k in m) {\
  \    ++s;\
  \  }\
  \  return s;\
  \}" :: forall a. StrMap a -> Number

singleton :: forall a. String -> a -> StrMap a
singleton k v = pureST (do
  s <- SM.new
  SM.poke s k v
  P.return s)

foreign import _lookup
  "function _lookup(no, yes, k, m) {\
  \  return k in m ? yes(m[k]) : no;\
  \}" :: forall a z. Fn4 z (a -> z) String (StrMap a) z

lookup :: forall a. String -> StrMap a -> Maybe a
lookup = runFn4 _lookup Nothing Just

member :: forall a. String -> StrMap a -> Boolean
member = runFn4 _lookup false (P.const true)

insert :: forall a. String -> a -> StrMap a -> StrMap a
insert k v = mutate (\s -> SM.poke s k v)

foreign import _unsafeDeleteStrMap
  "function _unsafeDeleteStrMap(m, k) { \
  \   delete m[k];                      \
  \   return m;                         \
  \}" :: forall a. Fn2 (StrMap a) String (StrMap a)

delete :: forall a. String -> StrMap a -> StrMap a
delete k = mutate (\s -> SM.delete s k)

alter :: forall a. (Maybe a -> Maybe a) -> String -> StrMap a -> StrMap a
alter f k m = case f (k `lookup` m) of
  Nothing -> delete k m
  Just v -> insert k v m

update :: forall a. (a -> Maybe a) -> String -> StrMap a -> StrMap a
update f k m = alter (maybe Nothing f) k m

fromList :: forall a. [Tuple String a] -> StrMap a
fromList l = pureST (do
  s <- SM.new
  for_ l (\(Tuple k v) -> SM.poke s k v)
  P.return s)

foreign import _collect
  "function _collect(f) {\
  \  return function (m) {\
  \    var r = [];\
  \    for (var k in m)\
  \      r.push(f(k)(m[k]));\
  \    return r;\
  \  };\
  \}" :: forall a b . (String -> a -> b) -> StrMap a -> [b]

toList :: forall a. StrMap a -> [Tuple String a]
toList = _collect Tuple

foreign import keys
  "var keys = Object.keys || _collect(function (k) {\
  \  return function () { return k; };\
  \});" :: forall a. StrMap a -> [String]

values :: forall a. StrMap a -> [a]
values = _collect (\_ v -> v)

-- left-biased
union :: forall a. StrMap a -> StrMap a -> StrMap a
union m = mutate (\s -> foldM SM.poke s m)

unions :: forall a. [StrMap a] -> StrMap a
unions = foldl union empty

map :: forall a b. (a -> b) -> StrMap a -> StrMap b
map = P.(<$>)

instance semigroupStrMap :: (P.Semigroup a) => P.Semigroup (StrMap a) where
  (<>) m1 m2 = mutate (\s -> foldM (\s k v2 -> SM.poke s k (runFn4 _lookup v2 (\v1 -> v1 P.<> v2) k m2)) s m1) m2
