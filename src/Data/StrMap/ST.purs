-- | Helper functions for working with mutable maps using the `ST` effect.
-- |
-- | This module can be used when performance is important and mutation is a local effect.

module Data.StrMap.ST
  ( STStrMap
  , new
  , peek
  , poke
  , delete
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST)
import Data.Maybe (Maybe(..))

-- | A reference to a mutable map
-- |
-- | The first type parameter represents the memory region which the map belongs to. The second type parameter defines the type of elements of the mutable array.
-- |
-- | The runtime representation of a value of type `STStrMap h a` is the same as that of `StrMap a`, except that mutation is allowed.
foreign import data STStrMap :: Type -> Type -> Type

-- | Create a new, empty mutable map
foreign import new :: forall a h r. Eff (st :: ST h | r) (STStrMap h a)

-- | Get the value for a key in a mutable map
peek :: forall a h r. STStrMap h a -> String -> Eff (st :: ST h | r) (Maybe a)
peek = peekImpl Just Nothing

foreign import peekImpl :: forall a b h r. (a -> b) -> b -> STStrMap h a -> String -> Eff (st :: ST h | r) b

-- | Update the value for a key in a mutable map
foreign import poke :: forall a h r. STStrMap h a -> String -> a -> Eff (st :: ST h | r) (STStrMap h a)

-- | Remove a key and the corresponding value from a mutable map
foreign import delete :: forall a h r. STStrMap h a -> String -> Eff (st :: ST h | r) (STStrMap h a)
