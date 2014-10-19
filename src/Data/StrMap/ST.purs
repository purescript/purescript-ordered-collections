module Data.StrMap.ST
  ( STStrMap()
  , new
  , peek
  , poke
  , delete
  ) where

import Control.Monad.Eff
import Control.Monad.ST
import Data.Maybe

foreign import data STStrMap :: * -> * -> *

foreign import _new """
  function _new() {
    return {};
  }""" :: forall a h r. Eff (st :: ST h | r) (STStrMap h a)

new :: forall a h r. Eff (st :: ST h | r) (STStrMap h a)
new = _new

foreign import peek """
  function peek(m) {
    return function (k) {
      return function () {
        return m[k];
      }
    }
  }""" :: forall a h r. STStrMap h a -> String -> Eff (st :: ST h | r) a

foreign import poke """
  function poke(m) {
    return function (k) {
      return function (v) {
        return function () {
          m[k] = v;
          return m;
        };
      };
    };
  }""" :: forall a h r. STStrMap h a -> String -> a -> Eff (st :: ST h | r) (STStrMap h a)

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
