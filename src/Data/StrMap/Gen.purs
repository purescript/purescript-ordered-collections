module Data.StrMap.Gen where

import Prelude

import Control.Monad.Gen (class MonadGen, chooseInt, resize, sized, unfoldable)
import Control.Monad.Rec.Class (class MonadRec)
import Data.StrMap (StrMap, fromFoldable)
import Data.Tuple (Tuple(..))
import Data.List (List)

-- | Generates a `StrMap` using the specified key and value generators.
genStrMap
  :: forall m a
  . MonadRec m
  => MonadGen m
  => m String
  -> m a
  -> m (StrMap a)
genStrMap genKey genValue = sized \size -> do
  newSize <- chooseInt 0 size
  resize (const newSize) $
    (fromFoldable :: List (Tuple String a) -> StrMap a)
      <$> unfoldable (Tuple <$> genKey <*> genValue)
