module Data.StrMap.ST.Unsafe
  ( unsafePeek
  ) where

import Control.Monad.Eff
import Control.Monad.ST
import Data.StrMap.Unsafe
import Data.StrMap.ST

foreign import unsafePeek """
  function unsafePeek(m) {
    return function (k) {
      return function () {
        return m[k];
      }
    }
  }""" :: forall a h r. STStrMap h a -> String -> Eff (st :: ST h | r) a
