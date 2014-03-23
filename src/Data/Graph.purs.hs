module Data.Graph (
  Edge(..),
  Graph(..),

  scc,
  topSort
  ) where

import Data.Maybe
import Data.Array
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
scc (Graph vs es) = runPure (runST (do
  index      <- newSTRef 0
  path       <- newSTRef []
  indexMap   <- newSTRef M.empty
  lowlinkMap <- newSTRef M.empty
  components <- newSTRef []

  (let 
    indexOf v = do
      m <- readSTRef indexMap
      return $ M.lookup v m
    
    lowlinkOf v = do
      m <- readSTRef lowlinkMap
      return $ M.lookup v m

    go [] = readSTRef components
    go (v : vs) = do
      currentIndex <- indexOf v
      when (isNothing currentIndex) $ strongConnect v
      go vs

    strongConnect v = do
      i <- readSTRef index

      modifySTRef indexMap   $ M.insert v i
      modifySTRef lowlinkMap $ M.insert v i

      writeSTRef index $ i + 1
      modifySTRef path $ (:) v

      for es $ \(Edge v' w) -> when (v == v') $ do
        wIndex <- indexOf w
        currentPath <- readSTRef path

        case wIndex of
          Nothing -> do
            strongConnect w
            wLowlink <- lowlinkOf w
            case wLowlink of
              Just lowlink -> do
                modifySTRef lowlinkMap $ M.alter (maybeMin lowlink) v
                return {}
              _ -> return {}
          _ -> when (w `elem` currentPath) $ do
                 wIndex <- indexOf w
                 case wIndex of
                   Just index -> do
                     modifySTRef lowlinkMap $ M.alter (maybeMin index) v
                     return {}
                   _ -> return {}

      vIndex <- indexOf v
      vLowlink <- lowlinkOf v        

      when (vIndex == vLowlink) $ do
        currentPath <- readSTRef path
        let newPath = popUntil v currentPath []
        modifySTRef components $ flip concat [newPath.component]
        writeSTRef path newPath.path
        return {}
    in go vs)))

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
