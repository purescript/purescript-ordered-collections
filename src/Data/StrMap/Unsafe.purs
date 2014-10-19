module Data.StrMap.Unsafe
  ( unsafeIndex
  ) where

import Data.StrMap

-- also known as (!)
foreign import unsafeIndex
  "function unsafeIndex(m) {              \
  \  return function (k) {\
  \    return m[k];\
  \  };\
  \}" :: forall a . StrMap a -> String -> a
