module Data.StrMap.ST.Unsafe where

import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST)
import Data.StrMap (StrMap)
import Data.StrMap.ST (STStrMap)

-- | Unsafely get the map out of ST without copying it
-- |
-- | If you later change the ST version of the map the pure value will also change.
foreign import unsafeFreeze :: forall a h r. STStrMap h a -> Eff (st :: ST h | r) (StrMap a)
