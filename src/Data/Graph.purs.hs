module Data.Graph (
  Edge(..),
  Graph(..),

  scc,
  topSort
  ) where

import Data.Maybe
import Data.Array
import Data.Foldable

import qualified Data.Map as M
import qualified Data.Set as S

data Edge v = Edge v v

data Graph v = Graph [v] [Edge v]

type Index = Number

type SCCState v =
  { index            :: Index
  , path             :: [v] 
  , indexMap         :: M.Map v Index
  , lowlinkMap       :: M.Map v Index
  , components       :: [[v]]
  }

scc :: forall v. (Eq v, Ord v) => Graph v -> [[v]]
scc (Graph vs es) = 
  let st = foldl (scc' es) initialSCCState vs
  in st.components

initialSCCState :: forall v. SCCState v
initialSCCState =
  { index            : 0
  , path             : []
  , indexMap         : M.empty
  , lowlinkMap       : M.empty 
  , components       : []
  }

scc' :: forall v. (Eq v, Ord v) => [Edge v] -> SCCState v -> v -> SCCState v
scc' es st v | M.lookup v st.indexMap == Nothing = 
  let indexOf   st v = M.lookup v st.indexMap in
  let lowlinkOf st v = M.lookup v st.lowlinkMap in

  let st1 = st { indexMap         = M.insert v st.index st.indexMap
               , lowlinkMap       = M.insert v st.index st.lowlinkMap
               , index            = st.index + 1 
               , path             = v : st.path
               } in
  let st2 = foldl (\st (Edge v' w) -> 
    if v == v'
    then 
      case indexOf st w of
        Nothing -> let st' = scc' es st w in
                   case lowlinkOf st' w of
                     Just lowlink -> st' { lowlinkMap = M.alter (maybeMin lowlink) v st'.lowlinkMap }
                     _ -> st'
        _ -> case w `elem` st.path of
          true -> case indexOf st w of
            Just index -> st { lowlinkMap = M.alter (maybeMin index) v st.lowlinkMap }
            _ -> st
          false -> st
    else st) st1 es in
  if st2 `indexOf` v == st2 `lowlinkOf` v
  then let newPath = popUntil v st2.path []
       in st2 { components = st2.components `concat` [newPath.component]
              , path = newPath.path
              }
  else st2 
scc' _ st _ = st

popUntil :: forall v. (Eq v) => v -> [v] -> [v] -> { path :: [v], component :: [v] }
popUntil _ [] popped = { path: [], component: popped } 
popUntil v (w : path) popped | v == w = { path: path, component: w : popped }
popUntil v (w : ws) popped = popUntil v ws (w : popped)

maybeMin :: Index -> Maybe Index -> Maybe Index
maybeMin i Nothing = Just i
maybeMin i (Just j) = Just $ Math.min i j

-- |
-- Topological sort
--
topSort :: forall v. (Eq v, Ord v) => Graph v -> [v]
topSort = reverse <<< concatMap id <<< scc
