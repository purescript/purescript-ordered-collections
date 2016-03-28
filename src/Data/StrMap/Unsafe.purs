module Data.StrMap.Unsafe
  ( unsafeIndex
  ) where

import Data.StrMap (StrMap)

-- | Unsafely get the value for a key in a map.
-- |
-- | This function does not check whether the key exists in the map.
foreign import unsafeIndex :: forall a. StrMap a -> String -> a
