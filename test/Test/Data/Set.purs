module Test.Data.Set where

import Prelude

import Data.Set (Set)
import Data.Set as S
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert)

setTests :: Effect Unit
setTests = do
  log "fromFoldable - empty"
  assert $ S.fromFoldable [] == S.empty :: Set Unit

  log "fromFoldable - non empty"
  do let set = S.fromFoldable [0, 1, 1, 2]
     assert $ S.size set == 3
     assert $ S.member 0 set
     assert $ S.member 1 set
     assert $ S.member 2 set

  log "intersection"
  do let s1 = S.fromFoldable [1,2,3,4,5]
         s2 = S.fromFoldable [2,4,6,8,10]
         s3 = S.fromFoldable [2,4]
     assert $ S.intersection s1 s2 == s3

  log "semiring"
  do let s1 = S.fromFoldable ["a", "b", "c"]
         s2 = S.fromFoldable ["d", "e"]
         s3 = S.fromFoldable ["f", "g", "h", "i"]
     log "- commutative monoid under addition"
     assert $ s1 + s2 == s2 + s1
     assert $ s1 + (s2 + s3) == (s1 + s2) + s3
     assert $ s1 + zero == s1

     log "- monoid under multiplication"
     assert $ s1 * one == s1
     assert $ one * s1 == s1
     assert $ s1 * (s2 * s3) == (s1 * s2) * s3

     log "- distributivity"
     assert $ s1 * (s2 + s3) == (s1 * s2) + (s1 * s3)
     assert $ (s1 + s2) * s3 == (s1 * s3) + (s2 * s3)

     log "- annihilation"
     assert $ zero * s1 == zero
     assert $ s1 * zero == zero
