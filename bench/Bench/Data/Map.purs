module Bench.Data.Map where

import Prelude

import Data.List as L
import Data.Map as M
import Bench.OldMap as OldMap
import Data.Foldable as F
import Data.FoldableWithIndex as FI
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Performance.Minibench (bench, benchWith)
import Data.Monoid.Additive (Additive(..))

benchMap :: Effect Unit
benchMap = do
  log "size"
  log "---------------"
  benchSize

  log ""

  log "fromFoldable"
  log "------------"
  benchFromFoldable

  log ""

  log "Foldable"
  log "---------------"
  benchFoldable

  log ""

  log "union"
  log "---------------"
  benchUnion

  where

  benchUnion = do
    let nats = L.range 0 999999
        nats2 = L.range 999999 1999999
        natPairs = (flip Tuple) unit <$> nats
        natPairs2 = (flip Tuple) unit <$> nats2
        bigMap = OldMap.fromFoldable $ natPairs
        bigMap2 = OldMap.fromFoldable $ natPairs2
        bigMap' = M.fromFoldable $ natPairs
        bigMap2' = M.fromFoldable $ natPairs2
    
    log $ "OldMap.union: big map (" <> show (OldMap.size bigMap) <> ")"
    benchWith 10 \_ -> OldMap.union bigMap bigMap2

    log $ "M.union: big map (" <> show (M.size bigMap') <> ")"
    benchWith 10 \_ -> M.union bigMap' bigMap2'

  benchSize = do
    let nats = L.range 0 999999
        natPairs = (flip Tuple) unit <$> nats
        singletonMap = M.singleton 0 unit
        smallMap = M.fromFoldable $ L.take 100 natPairs
        midMap = M.fromFoldable $ L.take 10000 natPairs
        bigMap = M.fromFoldable $ natPairs

    log "size: singleton map"
    bench \_ -> M.size singletonMap

    log $ "size: small map (" <> show (M.size smallMap) <> ")"
    bench \_ -> M.size smallMap

    log $ "size: midsize map (" <> show (M.size midMap) <> ")"
    benchWith 100 \_ -> M.size midMap

    log $ "size: big map (" <> show (M.size bigMap) <> ")"
    benchWith 10  \_ -> M.size bigMap

  benchFoldable = do
    let nats = L.range 0 999999
        natPairs = (\x -> Tuple x x) <$> nats
        bigMap = OldMap.fromFoldable $ natPairs
        bigMap' = M.fromFoldable $ natPairs

    log $ "OldMap.foldr big map (" <> show (OldMap.size bigMap) <> ")"
    benchWith 10 \_ -> F.foldr (+) 0 bigMap

    log $ "M.foldr big map (" <> show (M.size bigMap') <> ")"
    benchWith 10 \_ -> F.foldr (+) 0 bigMap'

    log $ "OldMap.foldl big map (" <> show (OldMap.size bigMap) <> ")"
    benchWith 10 \_ -> F.foldl (+) 0 bigMap

    log $ "M.foldl big map (" <> show (M.size bigMap') <> ")"
    benchWith 10 \_ -> F.foldl (+) 0 bigMap'

    log $ "OldMap.foldMap big map (" <> show (OldMap.size bigMap) <> ")"
    benchWith 10 \_ -> F.foldMap Additive bigMap

    log $ "M.foldMap big map (" <> show (M.size bigMap') <> ")"
    benchWith 10 \_ -> F.foldMap Additive bigMap'

    log $ "OldMap.foldrWithIndex big map (" <> show (OldMap.size bigMap) <> ")"
    benchWith 10 \_ -> FI.foldrWithIndex (\k v a -> k + a + v) 0 bigMap

    log $ "M.foldrWithIndex big map (" <> show (M.size bigMap') <> ")"
    benchWith 10 \_ -> FI.foldrWithIndex (\k v a -> k + a + v) 0 bigMap'

    log $ "OldMap.foldlWithIndex big map (" <> show (OldMap.size bigMap) <> ")"
    benchWith 10 \_ -> FI.foldlWithIndex (\k a v -> k + a + v) 0 bigMap

    log $ "M.foldlWithIndex big map (" <> show (M.size bigMap') <> ")"
    benchWith 10 \_ -> FI.foldlWithIndex (\k a v -> k + a + v) 0 bigMap'

    log $ "OldMap.foldMapWithIndex big map (" <> show (OldMap.size bigMap) <> ")"
    benchWith 10 \_ -> FI.foldMapWithIndex (\i v -> Additive i <> Additive v) bigMap

    log $ "M.foldMapWithIndex big map (" <> show (M.size bigMap') <> ")"
    benchWith 10 \_ -> FI.foldMapWithIndex (\i v -> Additive i <> Additive v) bigMap'

  benchFromFoldable = do
    let natStrs = show <$> L.range 0 99999
        natPairs = (flip Tuple) unit <$> natStrs
        shortPairList = L.take 10000 natPairs

    log $ "fromFoldable (" <> show (L.length shortPairList) <> ")"
    benchWith 100 \_ -> M.fromFoldable shortPairList

    log $ "fromFoldable (" <> show (L.length natPairs) <> ")"
    benchWith 10 \_ -> M.fromFoldable natPairs
