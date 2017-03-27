module Test.Data.StrMap where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)

import Data.Foldable (foldl)
import Data.Function (on)
import Data.List (List(..), groupBy, sortBy, singleton, fromFoldable, zipWith)
import Data.List.NonEmpty as NEL
import Data.NonEmpty ((:|))
import Data.Maybe (Maybe(..))
import Data.StrMap as M
import Data.Tuple (Tuple(..), fst)

import Partial.Unsafe (unsafePartial)

import Test.QuickCheck ((<?>), quickCheck, quickCheck', (===))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen as Gen

newtype TestStrMap v = TestStrMap (M.StrMap v)

instance arbTestStrMap :: (Arbitrary v) => Arbitrary (TestStrMap v) where
  arbitrary = TestStrMap <<< (M.fromFoldable :: List (Tuple String v) -> M.StrMap v) <$> arbitrary

data Instruction k v = Insert k v | Delete k

instance showInstruction :: (Show k, Show v) => Show (Instruction k v) where
  show (Insert k v) = "Insert (" <> show k <> ") (" <> show v <> ")"
  show (Delete k) = "Delete (" <> show k <> ")"

instance arbInstruction :: (Arbitrary v) => Arbitrary (Instruction String v) where
  arbitrary = do
    b <- arbitrary
    k <- Gen.frequency $ Tuple 10.0 (pure "hasOwnProperty") :| Tuple 50.0 arbitrary `Cons` Nil
    case b of
      true -> do
        v <- arbitrary
        pure (Insert k v)
      false -> do
        pure (Delete k)

runInstructions :: forall v. List (Instruction String v) -> M.StrMap v -> M.StrMap v
runInstructions instrs t0 = foldl step t0 instrs
  where
  step tree (Insert k v) = M.insert k v tree
  step tree (Delete k) = M.delete k tree

number :: Int -> Int
number n = n

strMapTests :: forall eff. Eff (console :: CONSOLE, random :: RANDOM, exception :: EXCEPTION | eff) Unit
strMapTests = do
  log "Test inserting into empty tree"
  quickCheck $ \k v -> M.lookup k (M.insert k v M.empty) == Just (number v)
    <?> ("k: " <> show k <> ", v: " <> show v)

  log "Test delete after inserting"
  quickCheck $ \k v -> M.isEmpty (M.delete k (M.insert k (number v) M.empty))
    <?> ("k: " <> show k <> ", v: " <> show v)

  log "Test pop after inserting"
  quickCheck $ \k v -> M.pop k (M.insert k (number v) M.empty) == Just (Tuple v M.empty)
    <?> ("k: " <> show k <> ", v: " <> show v)

  log "Pop non-existent key"
  quickCheck $ \k1 k2 v -> k1 == k2 || M.pop k2 (M.insert k1 (number v) M.empty) == Nothing
    <?> ("k1: " <> show k1 <> ", k2: " <> show k2 <> ", v: " <> show v)

  log "Insert two, lookup first"
  quickCheck $ \k1 v1 k2 v2 -> k1 == k2 || M.lookup k1 (M.insert k2 (number v2) (M.insert k1 (number v1) M.empty)) == Just v1
    <?> ("k1: " <> show k1 <> ", v1: " <> show v1 <> ", k2: " <> show k2 <> ", v2: " <> show v2)

  log "Insert two, lookup second"
  quickCheck $ \k1 v1 k2 v2 -> M.lookup k2 (M.insert k2 (number v2) (M.insert k1 (number v1) M.empty)) == Just v2
    <?> ("k1: " <> show k1 <> ", v1: " <> show v1 <> ", k2: " <> show k2 <> ", v2: " <> show v2)

  log "Insert two, delete one"
  quickCheck $ \k1 v1 k2 v2 -> k1 == k2 || M.lookup k2 (M.delete k1 (M.insert k2 (number v2) (M.insert k1 (number v1) M.empty))) == Just v2
    <?> ("k1: " <> show k1 <> ", v1: " <> show v1 <> ", k2: " <> show k2 <> ", v2: " <> show v2)

  log "Lookup from empty"
  quickCheck $ \k -> M.lookup k (M.empty :: M.StrMap Int) == Nothing

  log "Lookup from singleton"
  quickCheck $ \k v -> M.lookup k (M.singleton k (v :: Int)) == Just v

  log "Random lookup"
  quickCheck' 1000 $ \instrs k v ->
    let
      tree :: M.StrMap Int
      tree = M.insert k v (runInstructions instrs M.empty)
    in M.lookup k tree == Just v <?> ("instrs:\n  " <> show instrs <> "\nk:\n  " <> show k <> "\nv:\n  " <> show v)

  log "Singleton to list"
  quickCheck $ \k v -> M.toList (M.singleton k v :: M.StrMap Int) == singleton (Tuple k v)

  log "fromFoldable [] = empty"
  quickCheck (M.fromFoldable [] == (M.empty :: M.StrMap Unit)
    <?> "was not empty")

  log "fromFoldable & key collision"
  do
    let nums = M.fromFoldable [Tuple "0" "zero", Tuple "1" "what", Tuple "1" "one"]
    quickCheck (M.lookup "0" nums == Just "zero" <?> "invalid lookup - 0")
    quickCheck (M.lookup "1" nums == Just "one"  <?> "invalid lookup - 1")
    quickCheck (M.lookup "2" nums == Nothing     <?> "invalid lookup - 2")

  log "fromFoldableWith const [] = empty"
  quickCheck (M.fromFoldableWith const [] == (M.empty :: M.StrMap Unit)
    <?> "was not empty")

  log "fromFoldableWith (+) & key collision"
  do
    let nums = M.fromFoldableWith (+) [Tuple "0" 1, Tuple "1" 1, Tuple "1" 1]
    quickCheck (M.lookup "0" nums == Just 1  <?> "invalid lookup - 0")
    quickCheck (M.lookup "1" nums == Just 2  <?> "invalid lookup - 1")
    quickCheck (M.lookup "2" nums == Nothing <?> "invalid lookup - 2")

  log "toList . fromFoldable = id"
  quickCheck $ \arr -> let f x = M.toList (M.fromFoldable x)
                       in f (f arr) == f (arr :: List (Tuple String Int)) <?> show arr

  log "fromFoldable . toList = id"
  quickCheck $ \(TestStrMap m) ->
    let f m1 = M.fromFoldable (M.toList m1) in
    M.toList (f m) == M.toList (m :: M.StrMap Int) <?> show m

  log "fromFoldableWith const = fromFoldable"
  quickCheck $ \arr -> M.fromFoldableWith const arr ==
                       M.fromFoldable (arr :: List (Tuple String Int)) <?> show arr

  log "fromFoldableWith (<>) = fromFoldable . collapse with (<>) . group on fst"
  quickCheck $ \arr ->
    let combine (Tuple s a) (Tuple t b) = (Tuple s $ b <> a)
        foldl1 g = unsafePartial \(Cons x xs) -> foldl g x xs
        f = M.fromFoldable <<< map (foldl1 combine <<< NEL.toList) <<<
            groupBy ((==) `on` fst) <<< sortBy (compare `on` fst) in
    M.fromFoldableWith (<>) arr == f (arr :: List (Tuple String String)) <?> show arr

  log "Lookup from union"
  quickCheck $ \(TestStrMap m1) (TestStrMap m2) k ->
    M.lookup k (M.union m1 m2) == (case M.lookup k m1 of
      Nothing -> M.lookup k m2
      Just v -> Just (number v)) <?> ("m1: " <> show m1 <> ", m2: " <> show m2 <> ", k: " <> show k <> ", v1: " <> show (M.lookup k m1) <> ", v2: " <> show (M.lookup k m2) <> ", union: " <> show (M.union m1 m2))

  log "Union is idempotent"
  quickCheck $ \(TestStrMap m1) (TestStrMap m2) ->
    (m1 `M.union` m2) == ((m1 `M.union` m2) `M.union` (m2 :: M.StrMap Int)) <?> (show (M.size (m1 `M.union` m2)) <> " != " <> show (M.size ((m1 `M.union` m2) `M.union` m2)))

  log "fromFoldable = zip keys values"
  quickCheck $ \(TestStrMap m) -> M.toList m == zipWith Tuple (fromFoldable $ M.keys m) (M.values m :: List Int)

  log "mapWithKey is correct"
  quickCheck $ \(TestStrMap m :: TestStrMap Int) -> let
    f k v = k <> show v
    resultViaMapWithKey = m # M.mapWithKey f
    resultViaLists = m # M.toList # map (\(Tuple k v) → Tuple k (f k v)) # M.fromFoldable
    in resultViaMapWithKey === resultViaLists

  log "Bug #63: accidental observable mutation in foldMap"
  quickCheck \(TestStrMap m) ->
    let lhs = go m
        rhs = go m
    in lhs == rhs <?> ("lhs: " <> show lhs <> ", rhs: " <> show rhs)
    where
    go :: M.StrMap (Array Ordering) -> Array Ordering
    go = M.foldMap \_ v -> v
