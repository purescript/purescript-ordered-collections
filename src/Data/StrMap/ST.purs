module Data.StrMap.ST
  ( STStrMap()
  , new
  , freeze
  , thaw
  , isEmpty
  , peek
  , size
  , poke
  , delete
  ) where

import Control.Monad.Eff
import Control.Monad.ST
import Data.Maybe

import qualified Data.StrMap as SM

foreign import data STStrMap :: * -> * -> *

foreign import _new """
  function _new() {
    return {};
  }""" :: forall a h r. Eff (st :: ST h | r) (STStrMap h a)

new :: forall a h r. Eff (st :: ST h | r) (STStrMap h a)
new = _new

foreign import _copy """
  function _copy(m) {
    return function () {
      var r = {};
      for (var k in m)
        r[k] = m[k];
      return r;
    };
  }""" :: forall a b h r. a -> Eff (st :: ST h | r) b

thaw :: forall a h r. SM.StrMap a -> Eff (st :: ST h | r) (STStrMap h a)
thaw = _copy

freeze :: forall a h r. STStrMap h a -> Eff (st :: ST h | r) (SM.StrMap a)
freeze = _copy 

foreign import _unST """
  function _unST(m) {
    return m;
  }""" :: forall a h. STStrMap h a -> SM.StrMap a

isEmpty :: forall a h r. STStrMap h a -> Eff (st :: ST h | r) Boolean
isEmpty m = return (SM.isEmpty (_unST m))

peek :: forall a h r. STStrMap h a -> String -> Eff (st :: ST h | r) (Maybe a)
peek m k = return (SM.lookup k (_unST m))

size :: forall a h r. STStrMap h a -> Eff (st :: ST h | r) Number
size m = return (SM.size (_unST m))

foreign import poke """
  function poke(m) {
    return function (k) {
      return function (v) {
        return function () {
          return m[k] = v;
        };
      };
    };
  }""" :: forall a h r. STStrMap h a -> String -> a -> Eff (st :: ST h | r) a

foreign import _delete """
  function _delete(m) {
    return function (k) {
      return function () {
        delete m[k];
        return m;
      };
    };
  }""" :: forall a h r. STStrMap h a -> String -> Eff (st :: ST h | r) (STStrMap h a)

delete :: forall a h r. STStrMap h a -> String -> Eff (st :: ST h | r) (STStrMap h a)
delete = _delete

foreign import run """
  function run(f) {
    return f;
  }""" :: forall a r. (forall h. Eff (st :: ST h | r) (STStrMap h a)) -> Eff r (SM.StrMap a)
