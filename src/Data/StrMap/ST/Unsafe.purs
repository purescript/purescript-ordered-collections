module Data.StrMap.ST.Unsafe where

import Data.StrMap (StrMap)
import Data.StrMap.ST (STStrMap)

-- | Unsafely get the map out of ST without copying it
-- |
-- | If you later change the ST version of the map the pure value will also change.
foreign import unsafeFreeze :: forall a h. STStrMap h a -> StrMap a
