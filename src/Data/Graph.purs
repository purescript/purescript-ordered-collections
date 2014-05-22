module Data.Graph (
  Edge(..),
  Graph(..),

  scc,
  scc',
  
  topSort,
  topSort'
  ) where

import Data.Maybe
import Data.Array (map, reverse, concatMap)
import Data.Foldable
import Data.Traversable

import Control.Monad
import Control.Monad.Eff
import Control.Monad.ST

import qualified Data.Map as M
import qualified Data.Set as S

data Edge k = Edge k k

data Graph k v = Graph [v] [Edge k]

type Index = Number

scc :: forall v. (Eq v, Ord v) => Graph v v -> [[v]]
scc = scc' id id

scc' :: forall k v. (Eq k, Ord k) => (v -> k) -> (k -> v) -> Graph k v -> [[v]]
scc' makeKey makeVert (Graph vs es) = runPure (runST (do
  index      <- newSTRef 0
  path       <- newSTRef []
  indexMap   <- newSTRef M.empty
  lowlinkMap <- newSTRef M.empty
  components <- newSTRef []

  (let 
    indexOf v = indexOfKey (makeKey v)
      
    indexOfKey k = do
      m <- readSTRef indexMap
      return $ M.lookup k m
    
    lowlinkOf v = lowlinkOfKey (makeKey v)
      
    lowlinkOfKey k = do
      m <- readSTRef lowlinkMap
      return $ M.lookup k m

    go [] = readSTRef components
    go (v : vs) = do
      currentIndex <- indexOf v
      when (isNothing currentIndex) $ strongConnect (makeKey v)
      go vs

    strongConnect k = do
      let v = makeVert k
      
      i <- readSTRef index

      modifySTRef indexMap   $ M.insert k i
      modifySTRef lowlinkMap $ M.insert k i

      writeSTRef index $ i + 1
      modifySTRef path $ (:) v

      for es $ \(Edge k' l) -> when (k == k') $ do
        wIndex <- indexOfKey l
        currentPath <- readSTRef path

        case wIndex of
          Nothing -> do
            let w = makeVert l
            strongConnect l
            wLowlink <- lowlinkOfKey l
            for_ wLowlink $ \lowlink ->
              modifySTRef lowlinkMap $ M.alter (maybeMin lowlink) k
          _ -> when (l `elem` map makeKey currentPath) $ do
                 wIndex <- indexOfKey l
                 for_ wIndex $ \index ->
                   modifySTRef lowlinkMap $ M.alter (maybeMin index) k

      vIndex <- indexOfKey k
      vLowlink <- lowlinkOfKey k        

      when (vIndex == vLowlink) $ do
        currentPath <- readSTRef path
        let newPath = popUntil makeKey v currentPath []
        modifySTRef components $ flip (++) [newPath.component]
        writeSTRef path newPath.path
        return {}
    in go vs)))

popUntil :: forall k v. (Eq k) => (v -> k) -> v -> [v] -> [v] -> { path :: [v], component :: [v] }
popUntil _       _ []         popped = { path: [], component: popped } 
popUntil makeKey v (w : path) popped | makeKey v == makeKey w = { path: path, component: w : popped }
popUntil makeKey v (w : ws)   popped = popUntil makeKey v ws (w : popped)

maybeMin :: Index -> Maybe Index -> Maybe Index
maybeMin i Nothing = Just i
maybeMin i (Just j) = Just $ Math.min i j

-- |
-- Topological sort
--
topSort :: forall v. (Eq v, Ord v) => Graph v v -> [v]
topSort = topSort' id id

topSort' :: forall k v. (Eq k, Ord k) => (v -> k) -> (k -> v) -> Graph k v -> [v]
topSort' makeKey makeVert = reverse <<< concatMap id <<< scc' makeKey makeVert
