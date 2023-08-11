module Bench.Data.Map where

import Prelude

import Data.List as L
import Data.Map as M
import Bench.Data.Mapf149d5 as Mapf149d5
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

  log "eq"
  log "------------"
  benchEq

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

  log ""

  log "difference"
  log "---------------"
  benchDifference

  where
  nats = L.range 0 999999
  nats2 = L.range 999999 1999999
  natPairs = (flip Tuple) unit <$> nats
  natPairs2 = (flip Tuple) unit <$> nats2
  bigMap = Mapf149d5.fromFoldable $ natPairs
  bigMap2 = Mapf149d5.fromFoldable $ natPairs2
  bigMap' = M.fromFoldable $ natPairs
  bigMap2' = M.fromFoldable $ natPairs2
  singletonMap = M.singleton 0 unit
  smallMap = Mapf149d5.fromFoldable $ L.take 100 natPairs
  smallMap' = M.fromFoldable $ L.take 100 natPairs
  midMap = Mapf149d5.fromFoldable $ L.take 10000 natPairs
  midMap' = M.fromFoldable $ L.take 10000 natPairs
  size = Mapf149d5.size bigMap
  size' = M.size bigMap'

  benchUnion = do
    log $ "Mapf149d5.union: big map (" <> show size <> ")"
    benchWith 10 \_ -> Mapf149d5.union bigMap bigMap2

    log $ "M.union: big map (" <> show size' <> ")"
    benchWith 10 \_ -> M.union bigMap' bigMap2'

  benchValues = do
    log $ "Mapf149d5.values: big map (" <> show size <> ")"
    benchWith 10 \_ -> Mapf149d5.values bigMap

    log $ "M.values: big map (" <> show size' <> ")"
    benchWith 10 \_ -> M.values bigMap'

  benchKeys = do
    log $ "Mapf149d5.keys: big map (" <> show size <> ")"
    benchWith 10 \_ -> Mapf149d5.keys bigMap

    log $ "M.keys: big map (" <> show size' <> ")"
    benchWith 10 \_ -> M.keys bigMap'

  benchSize = do
    log "size: singleton map"
    bench \_ -> M.size singletonMap

    log $ "size: small map (" <> show (M.size smallMap') <> ")"
    bench \_ -> M.size smallMap'

    log $ "size: midsize map (" <> show (M.size midMap') <> ")"
    benchWith 100 \_ -> M.size midMap'

    log $ "size: big map (" <> show (M.size bigMap') <> ")"
    benchWith 10  \_ -> M.size bigMap'

  benchFoldable = do
    log $ "Mapf149d5.foldr big map (" <> show size <> ")"
    benchWith 10 \_ -> F.foldr (\_ _ -> unit) unit bigMap

    log $ "M.foldr big map (" <> show size' <> ")"
    benchWith 10 \_ -> F.foldr (\_ _ -> unit) unit bigMap'

    log $ "Mapf149d5.foldl big map (" <> show size <> ")"
    benchWith 10 \_ -> F.foldl (\_ _ -> unit) unit bigMap

    log $ "M.foldl big map (" <> show size' <> ")"
    benchWith 10 \_ -> F.foldl (\_ _ -> unit) unit bigMap'

    log $ "Mapf149d5.foldMap big map (" <> show size <> ")"
    benchWith 10 \_ -> F.foldMap (\_ -> unit) bigMap

    log $ "M.foldMap big map (" <> show size' <> ")"
    benchWith 10 \_ -> F.foldMap (\_ -> unit) bigMap'

    log $ "Mapf149d5.foldrWithIndex big map (" <> show size <> ")"
    benchWith 10 \_ -> FI.foldrWithIndex (\_ _ _ -> unit) unit bigMap

    log $ "M.foldrWithIndex big map (" <> show size' <> ")"
    benchWith 10 \_ -> FI.foldrWithIndex (\_ _ _ -> unit) unit bigMap'

    log $ "Mapf149d5.foldlWithIndex big map (" <> show size <> ")"
    benchWith 10 \_ -> FI.foldlWithIndex (\_ _ _ -> unit) unit bigMap

    log $ "M.foldlWithIndex big map (" <> show size' <> ")"
    benchWith 10 \_ -> FI.foldlWithIndex (\_ _ _ -> unit) unit bigMap'

    log $ "Mapf149d5.foldMapWithIndex big map (" <> show size <> ")"
    benchWith 10 \_ -> FI.foldMapWithIndex (\_ _ -> unit) bigMap

    log $ "M.foldMapWithIndex big map (" <> show size' <> ")"
    benchWith 10 \_ -> FI.foldMapWithIndex (\_ _ -> unit) bigMap'

  benchFromFoldable = do
    let natStrs = show <$> nats
        natStrsPairs = (flip Tuple) unit <$> natStrs
        shortPairList = L.take 10000 natStrsPairs

    log $ "fromFoldable (" <> show (L.length shortPairList) <> ")"
    benchWith 100 \_ -> M.fromFoldable shortPairList

    log $ "fromFoldable (" <> show (L.length natPairs) <> ")"
    benchWith 10 \_ -> M.fromFoldable natPairs

  benchDifference = do
    log $ "Mapf149d5.difference: small map (" <> show (Mapf149d5.size smallMap) <> ")"
    bench \_ -> Mapf149d5.difference smallMap midMap

    log $ "M.difference: small map (" <> show (M.size smallMap') <> ")"
    bench \_ -> M.difference smallMap' midMap'

    log $ "Mapf149d5.difference: midsize map (" <> show (Mapf149d5.size midMap) <> ")"
    benchWith 100 \_ -> Mapf149d5.difference midMap midMap

    log $ "M.difference: midsize map (" <> show (M.size midMap') <> ")"
    benchWith 100 \_ -> M.difference midMap' midMap'

    log $ "Mapf149d5.difference: big map (" <> show (Mapf149d5.size bigMap) <> ")"
    benchWith 10  \_ -> Mapf149d5.difference bigMap midMap

    log $ "M.difference: big map (" <> show (M.size bigMap') <> ")"
    benchWith 10  \_ -> M.difference bigMap' midMap'

  benchEq = do
    log $ "Mapf149d5.eq: small map (" <> show (Mapf149d5.size smallMap) <> ")"
    bench \_ -> smallMap == smallMap

    log $ "M.eq: small map (" <> show (M.size smallMap') <> ")"
    bench \_ -> smallMap' == smallMap'

    log $ "Mapf149d5.eq: midsize map (" <> show (Mapf149d5.size midMap) <> ")"
    benchWith 100 \_ -> midMap == midMap

    log $ "M.eq: midsize map (" <> show (M.size midMap') <> ")"
    benchWith 100 \_ -> midMap' == midMap'

    log $ "Mapf149d5.eq: big map (" <> show (Mapf149d5.size bigMap) <> ")"
    benchWith 10  \_ -> bigMap == bigMap

    log $ "M.eq: big map (" <> show (M.size bigMap') <> ")"
    benchWith 10  \_ -> bigMap' == bigMap'
