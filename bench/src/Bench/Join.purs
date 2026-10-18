module Bench.Join (main) where

import Prelude

import Data.Foldable (foldl, for_, sum)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as S
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Performance.Minibench (benchWith)

-- Keep the last result reachable so the runtime cannot discard the work.
foreign import retainResult :: forall a. (Unit -> a) -> Unit -> a
foreign import clearResult :: Effect Unit

benchmark :: forall a. String -> (a -> Boolean) -> (Unit -> a) -> Effect Unit
benchmark name valid operation = do
  log name
  let run = retainResult operation
  -- Validate and warm up outside the timed samples. Minibench then requests GC.
  for_ (L.range 1 5) \_ ->
    unless (valid (run unit)) $ throw ("Invalid benchmark result: " <> name)
  clearResult
  benchWith 20 run
  clearResult

main :: Effect Unit
main = for_ [ 1000, 100000 ] \n -> do
  log $ "\nJoin workloads, entries = " <> show n
  let
    keys = L.range 0 (n - 1)
    reverseKeys = L.reverse keys
    permutedKeys = map (\k -> (k * 7919) `mod` n) keys
    duplicateKeys = map (_ `mod` 128) keys
    pairs = map (\k -> Tuple k k) keys
    permutedPairs = map (\k -> Tuple k k) permutedKeys
    reversePairs = L.reverse pairs
    duplicatePairs = map (\k -> Tuple k k) duplicateKeys
    arrayPairs = L.toUnfoldable permutedPairs :: Array (Tuple Int Int)
    fromKeys = M.fromFoldable <<< map (\k -> Tuple k k)
    byUnions = foldl (\m k -> M.union m (M.singleton k k)) M.empty
    byLeftUnions = foldl (\m k -> M.union (M.singleton k k) m) M.empty
    valid expected m = M.checkValid m && (M.toUnfoldable m :: L.List (Tuple Int Int)) == expected
    mapBench name expected = benchmark name (valid expected)
    balanced = fromKeys keys
    ascending = byUnions keys
    descending = byUnions reverseKeys
    half = n / 2
    overlap = fromKeys (L.range half (n + half - 1))
    disjoint = fromKeys (L.range n (2 * n - 1))
    sparseKeys = map (_ * 97) (L.range 0 ((n - 1) / 97))
    sparse = fromKeys sparseKeys
    queries = map (\i -> (i * 7919) `mod` n) (L.range 0 4095)
    expectedLookup = sum queries
    lookups m = foldl (\acc k -> acc + fromMaybe (-1) (M.lookup k m)) 0 queries
    fullRange = M.submap (Just 0) (Just (n - 1)) balanced
    extraKeys = L.range n (n + 127)
    deletedKeys = L.range 0 127

  mapBench "fromFoldable" pairs \_ -> M.fromFoldable pairs
  mapBench "fromFoldable / permuted" pairs \_ -> M.fromFoldable permutedPairs
  mapBench "fromFoldable / descending" pairs \_ -> M.fromFoldable reversePairs
  mapBench "fromFoldable / duplicate-heavy" (L.take 128 pairs) \_ -> M.fromFoldable duplicatePairs
  mapBench "fromFoldable / permuted array" pairs \_ -> M.fromFoldable arrayPairs
  benchmark "Set.fromFoldable / permuted"
    (\s -> S.checkValid s && (S.toUnfoldable s :: L.List Int) == keys)
    \_ -> S.fromFoldable permutedKeys
  benchmark "Set.fromFoldable / duplicate-heavy"
    (\s -> S.checkValid s && (S.toUnfoldable s :: L.List Int) == L.take 128 keys)
    \_ -> S.fromFoldable duplicateKeys
  mapBench "insert / replace existing" (map (\k -> Tuple k (-k)) keys) \_ ->
    foldl (\m k -> M.insert k (-k) m) balanced permutedKeys
  mapBench "insertWith / replace existing" (map (\k -> Tuple k (-1)) keys) \_ ->
    foldl (\m k -> M.insertWith (-) k (k + 1) m) balanced permutedKeys
  mapBench "delete then reinsert" pairs \_ ->
    foldl (\m k -> M.insert k k (M.delete k m)) balanced permutedKeys
  mapBench "singleton unions / ascending" pairs \_ -> byUnions keys
  mapBench "singleton unions / descending" pairs \_ -> byUnions reverseKeys
  mapBench "singleton unions / permuted" pairs \_ -> byUnions permutedKeys
  mapBench "singleton unions / duplicate-heavy" (L.take 128 pairs) \_ -> byUnions duplicateKeys
  mapBench "singleton-left unions / ascending" pairs \_ -> byLeftUnions keys
  mapBench "singleton-left unions / descending" pairs \_ -> byLeftUnions reverseKeys
  mapBench "singleton-left unions / permuted" pairs \_ -> byLeftUnions permutedKeys
  mapBench "submap / full" pairs \_ -> M.submap (Just 0) (Just (n - 1)) balanced
  mapBench "submap / half" (L.take half pairs) \_ -> M.submap (Just 0) (Just (half - 1)) balanced
  mapBench "submap / singleton" (L.singleton (Tuple half half)) \_ -> M.submap (Just half) (Just half) balanced
  mapBench "submap / interior" (L.take half (L.drop (half / 2) pairs)) \_ ->
    M.submap (Just (half / 2)) (Just (half / 2 + half - 1)) balanced
  mapBench "submap / lower only" (L.drop half pairs) \_ -> M.submap (Just half) Nothing balanced
  mapBench "submap / upper only" (L.take half pairs) \_ -> M.submap Nothing (Just (half - 1)) balanced
  mapBench "submap / reversed bounds" L.Nil \_ -> M.submap (Just half) (Just (half - 1)) balanced
  mapBench "submap / outside" L.Nil \_ -> M.submap (Just n) Nothing balanced

  mapBench "union / identical" pairs \_ -> M.union balanced balanced
  mapBench "union / empty left" pairs \_ -> M.union M.empty balanced
  mapBench "union / empty right" pairs \_ -> M.union balanced M.empty
  mapBench "difference / identical" L.Nil \_ -> M.difference balanced balanced

  for_ [ Tuple "left" identity, Tuple "right" flip ] \(Tuple side order) -> do
    mapBench ("union / overlap / " <> side) (map (\k -> Tuple k k) (L.range 0 (n + half - 1))) \_ ->
      order M.union balanced overlap
    mapBench ("union / disjoint / " <> side) (pairs <> map (\k -> Tuple k k) (L.range n (2 * n - 1))) \_ ->
      order M.union balanced disjoint
    mapBench ("union / sparse / " <> side) pairs \_ -> order M.union balanced sparse

  mapBench "difference / disjoint / left" pairs \_ -> M.difference balanced disjoint
  mapBench "difference / disjoint / right" (map (\k -> Tuple k k) (L.range n (2 * n - 1))) \_ -> M.difference disjoint balanced
  mapBench "difference / sparse / large-small" (L.filter (\(Tuple k _) -> k `mod` 97 /= 0) pairs) \_ -> M.difference balanced sparse
  mapBench "difference / sparse / small-large" L.Nil \_ -> M.difference sparse balanced
  mapBench "intersection / overlap" (L.drop half pairs) \_ -> M.intersection balanced overlap

  for_ [ Tuple "fromFoldable" balanced, Tuple "ascending unions" ascending, Tuple "descending unions" descending, Tuple "full submap" fullRange ] \(Tuple name tree) -> do
    unless (valid pairs tree) $ throw ("Invalid lookup fixture: " <> name)
    benchmark ("4096 lookups / " <> name) (_ == expectedLookup) \_ -> lookups tree
    mapBench ("128 deletes / " <> name) (L.drop 128 pairs) \_ ->
      foldl (flip M.delete) tree deletedKeys
    mapBench ("128 inserts / " <> name) (pairs <> map (\k -> Tuple k k) extraKeys) \_ ->
      foldl (\m k -> M.insert k k m) tree extraKeys

  for_ [ Tuple "ascending" keys, Tuple "descending" reverseKeys ] \(Tuple name input) ->
    benchmark ("singleton unions + 4096 lookups / " <> name)
      (\(Tuple tree total) -> valid pairs tree && total == expectedLookup)
      \_ -> do
        let tree = byUnions input
        Tuple tree (lookups tree)
