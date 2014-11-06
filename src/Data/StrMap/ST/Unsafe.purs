module Data.StrMap.ST.Unsafe
  ( unsafeGet
  ) where

import Control.Monad.Eff (Eff())
import Control.Monad.ST (ST())
import Data.StrMap (StrMap())
import Data.StrMap.ST (STStrMap())

foreign import unsafeGet
  """
  function unsafeGet(m) {
    return function() {
      return m;
    }
  }
  """ :: forall a h r. STStrMap h a -> Eff (st :: ST h | r) (StrMap a)
