module Test.Data.Map.Unbiased where

import Prelude

import Data.Map.Unbiased as Unbiased
import Data.Map as M
import Data.Semigroup.First (First(..))
import Data.Semigroup.Last (Last(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck ((===), quickCheck)

singleton :: forall key value. key -> value -> Unbiased.Map key value
singleton k v = Unbiased.Map (M.singleton k v)

mapTests :: Effect Unit
mapTests = do
  log "Semigroup instance is based on value's Semigroup instance"
  quickCheck \(Tuple leftStr rightStr :: Tuple String String) -> do
    let key = "foo"
    let left = singleton key leftStr
    let right = singleton key rightStr
    let result = left <> right
    let expected = singleton key $ leftStr <> rightStr
    result == expected
  quickCheck \(Tuple leftStr rightStr :: Tuple String String) -> do
    let key = "foo"
    let left = singleton key $ First leftStr
    let right = singleton key $ First rightStr
    let result = left <> right
    result == left
  quickCheck \(Tuple leftStr rightStr :: Tuple String String) -> do
    let key = "foo"
    let left = singleton key $ Last leftStr
    let right = singleton key $ Last rightStr
    let result = left <> right
    result == right
