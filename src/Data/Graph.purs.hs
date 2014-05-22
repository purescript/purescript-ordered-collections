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

data Edge v = Edge v v

data Graph v = Graph [v] [Edge v]

type Index = Number

scc :: forall v. (Eq v, Ord v) => Graph v -> [[v]]
scc = scc' id

scc' :: forall k v. (Eq k, Ord k) => (v -> k) -> Graph v -> [[v]]
scc' makeKey (Graph vs es) = runPure (runST (do
  index      <- newSTRef 0
  path       <- newSTRef []
  indexMap   <- newSTRef M.empty
  lowlinkMap <- newSTRef M.empty
  components <- newSTRef []

  (let 
    indexOf v = do
      m <- readSTRef indexMap
      return $ M.lookup (makeKey v) m
    
    lowlinkOf v = do
      m <- readSTRef lowlinkMap
      return $ M.lookup (makeKey v) m

    go [] = readSTRef components
    go (v : vs) = do
      currentIndex <- indexOf v
      when (isNothing currentIndex) $ strongConnect v
      go vs

    strongConnect v = do
      i <- readSTRef index

      modifySTRef indexMap   $ M.insert (makeKey v) i
      modifySTRef lowlinkMap $ M.insert (makeKey v) i

      writeSTRef index $ i + 1
      modifySTRef path $ (:) v

      for es $ \(Edge v' w) -> when (makeKey v == makeKey v') $ do
        wIndex <- indexOf w
        currentPath <- readSTRef path

        case wIndex of
          Nothing -> do
            strongConnect w
            wLowlink <- lowlinkOf w
            for_ wLowlink $ \lowlink ->
              modifySTRef lowlinkMap $ M.alter (maybeMin lowlink) (makeKey v)
          _ -> when (makeKey w `elem` map makeKey currentPath) $ do
                 wIndex <- indexOf w
                 for_ wIndex $ \index ->
                   modifySTRef lowlinkMap $ M.alter (maybeMin index) (makeKey v)

      vIndex <- indexOf v
      vLowlink <- lowlinkOf v        

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
topSort :: forall v. (Eq v, Ord v) => Graph v -> [v]
topSort = topSort' id

topSort' :: forall k v. (Eq k, Ord k) => (v -> k) -> Graph v -> [v]
topSort' makeKey = reverse <<< concatMap id <<< scc' makeKey
