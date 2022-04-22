module Bench.Data.Map where

import Prelude

import Data.List as L
import Data.Map as M
import Bench.Data.Map2a0bff as Map2a0bff
import Data.Foldable as F
import Data.FoldableWithIndex as FI
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Performance.Minibench (bench, benchWith)

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

  log ""

  log "values"
  log "---------------"
  benchValues

  log ""

  log "keys"
  log "---------------"
  benchKeys

  where

  benchUnion = do
    let nats = L.range 0 999999
        nats2 = L.range 999999 1999999
        natPairs = (flip Tuple) unit <$> nats
        natPairs2 = (flip Tuple) unit <$> nats2
        bigMap = Map2a0bff.fromFoldable $ natPairs
        bigMap2 = Map2a0bff.fromFoldable $ natPairs2
        bigMap' = M.fromFoldable $ natPairs
        bigMap2' = M.fromFoldable $ natPairs2
        size = Map2a0bff.size bigMap
        size' = M.size bigMap'
    
    log $ "Map2a0bff.union: big map (" <> show size <> ")"
    benchWith 10 \_ -> Map2a0bff.union bigMap bigMap2

    log $ "M.union: big map (" <> show size' <> ")"
    benchWith 10 \_ -> M.union bigMap' bigMap2'

  benchValues = do
    let nats = L.range 0 999999
        natPairs = (flip Tuple) unit <$> nats
        bigMap = Map2a0bff.fromFoldable $ natPairs
        bigMap' = M.fromFoldable $ natPairs
        size = Map2a0bff.size bigMap
        size' = M.size bigMap'

    log $ "Map2a0bff.values: big map (" <> show size <> ")"
    benchWith 10 \_ -> Map2a0bff.values bigMap

    log $ "M.values: big map (" <> show size' <> ")"
    benchWith 10 \_ -> M.values bigMap'

  benchKeys = do
    let nats = L.range 0 999999
        natPairs = (flip Tuple) unit <$> nats
        bigMap = Map2a0bff.fromFoldable $ natPairs
        bigMap' = M.fromFoldable $ natPairs
        size = Map2a0bff.size bigMap
        size' = M.size bigMap'

    log $ "Map2a0bff.keys: big map (" <> show size <> ")"
    benchWith 10 \_ -> Map2a0bff.keys bigMap

    log $ "M.keys: big map (" <> show size' <> ")"
    benchWith 10 \_ -> M.keys bigMap'

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
        natPairs = (flip Tuple) unit <$> nats
        bigMap = Map2a0bff.fromFoldable $ natPairs
        bigMap' = M.fromFoldable $ natPairs
        size = Map2a0bff.size bigMap
        size' = M.size bigMap'

    log $ "Map2a0bff.foldr big map (" <> show size <> ")"
    benchWith 10 \_ -> F.foldr (\_ _ -> unit) unit bigMap

    log $ "M.foldr big map (" <> show size' <> ")"
    benchWith 10 \_ -> F.foldr (\_ _ -> unit) unit bigMap'

    log $ "Map2a0bff.foldl big map (" <> show size <> ")"
    benchWith 10 \_ -> F.foldl (\_ _ -> unit) unit bigMap

    log $ "M.foldl big map (" <> show size' <> ")"
    benchWith 10 \_ -> F.foldl (\_ _ -> unit) unit bigMap'

    log $ "Map2a0bff.foldMap big map (" <> show size <> ")"
    benchWith 10 \_ -> F.foldMap (\_ -> unit) bigMap

    log $ "M.foldMap big map (" <> show size' <> ")"
    benchWith 10 \_ -> F.foldMap (\_ -> unit) bigMap'

    log $ "Map2a0bff.foldrWithIndex big map (" <> show size <> ")"
    benchWith 10 \_ -> FI.foldrWithIndex (\_ _ _ -> unit) unit bigMap

    log $ "M.foldrWithIndex big map (" <> show size' <> ")"
    benchWith 10 \_ -> FI.foldrWithIndex (\_ _ _ -> unit) unit bigMap'

    log $ "Map2a0bff.foldlWithIndex big map (" <> show size <> ")"
    benchWith 10 \_ -> FI.foldlWithIndex (\_ _ _ -> unit) unit bigMap

    log $ "M.foldlWithIndex big map (" <> show size' <> ")"
    benchWith 10 \_ -> FI.foldlWithIndex (\_ _ _ -> unit) unit bigMap'

    log $ "Map2a0bff.foldMapWithIndex big map (" <> show size <> ")"
    benchWith 10 \_ -> FI.foldMapWithIndex (\_ _ -> unit) bigMap

    log $ "M.foldMapWithIndex big map (" <> show size' <> ")"
    benchWith 10 \_ -> FI.foldMapWithIndex (\_ _ -> unit) bigMap'

  benchFromFoldable = do
    let natStrs = show <$> L.range 0 99999
        natPairs = (flip Tuple) unit <$> natStrs
        shortPairList = L.take 10000 natPairs

    log $ "fromFoldable (" <> show (L.length shortPairList) <> ")"
    benchWith 100 \_ -> M.fromFoldable shortPairList

    log $ "fromFoldable (" <> show (L.length natPairs) <> ")"
    benchWith 10 \_ -> M.fromFoldable natPairs
