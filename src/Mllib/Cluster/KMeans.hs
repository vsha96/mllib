module Mllib.Cluster.KMeans
    ( Kmeans (labels, clusterCenters)
    , kmeans
    ) where

import Mllib.Types

import System.Random (StdGen, randomRs)

import Data.Foldable (foldMap)
import Data.List (elemIndices)
import Data.Monoid (Sum(..))



-- | Kmeans data type
data Kmeans = Kmeans
  { rGen           :: !StdGen     -- ^ Random generator
  , labels         :: ![Int]      -- ^ Indices of each point
  , clusterCenters :: ![Vector R] -- ^ Coordinates of cluster centers
  }
  deriving Show



{- | Kmeans main function.
     Returns type 'Kmeans' with labels and cluster centers inside.
-}
kmeans
    :: [Vector R] -- ^ List of vectors of double
    -> Int        -- ^ Number of clusters
    -> StdGen     -- ^ RandomGen
    -> Kmeans
kmeans list numOfClust gen = Kmeans gen labels clusterCenters
  where
    randomSeqOfIndices = randomRs (0, (length list) - 1) gen
    randomClusterIndices = takeUniqueIndices numOfClust randomSeqOfIndices
    initClusters = [list !! i | i <- randomClusterIndices]
    initLabels = assignIndices initClusters list
    (labels, clusterCenters) = kmeansLoopWithoutEps list initLabels

{- | Kmeans loop with manual error estimation.
     Returns labels and cluster centers as a tuple.
-}
kmeansLoop
    :: [Vector R] -- ^ Vectors
    -> [Vector R] -- ^ Old cluster centers
    -> R          -- ^ Epsilon, error
    -> ([Int], [Vector R])
kmeansLoop list oldCenters eps
    | maximum (map norm_2 (zipWith (-) oldCenters newCenters)) < eps = (labels, newCenters)
    | otherwise = kmeansLoop list newCenters eps
  where
    labels = assignIndices list oldCenters
    newCenters = computeClusters labels list 
    
{- | Runs kmeans loop until stabilization of clusters.
     Returns labels and cluster centers as a tuple.
     Stops when lists of labels become equal.
-}
kmeansLoopWithoutEps
    :: [Vector R]          -- ^ Vectors
    -> [Int]               -- ^ Old labels
    -> ([Int], [Vector R]) -- ^ New labels and cluster centers
kmeansLoopWithoutEps  list oldLabels
    | oldLabels == newLabels = (newLabels, newClusters)
    | otherwise              = kmeansLoopWithoutEps  list newLabels
  where
    newClusters = computeClusters oldLabels list
    newLabels = assignIndices newClusters list

-- TODO solve there a problem from numOfClust 
computeClusters
    :: [Int]      -- ^ Labels
    -> [Vector R] -- ^ Vectors
    -> [Vector R] -- ^ Centers of clusters
computeClusters indices list = 
    map (\(s, n) -> getSum s / getSum n) -- mean from the trick below
    [ foldMap (\x -> (Sum (snd x), Sum 1)) -- trick to count as (SumOfVecs, NumberOfVecs)
    $ filter (\tuple -> fst tuple == i) indexAndVector -- gather vectors by an index
    | i <- [0..(numOfClust-1)] ] -- compute for each cluster
  where
    numOfClust = (maximum indices) + 1
    indexAndVector = zipWith (\x y -> (x, y)) indices list

-- TODO ?rewrite: elemIndices (minimum listOfDists) listOfDists)
--             -> elemIndex (foldl1 min a) a
assignIndices 
    :: [Vector R] -- ^ Cluster centers
    -> [Vector R] -- ^ Vectors
    -> [Int]
assignIndices clusterCenters list = 
    map ((\listOfDists -> head $ elemIndices (minimum listOfDists) listOfDists) .
        map norm_2 . 
        zipWith (-) clusterCenters .
        replicate (length clusterCenters)) list

{- | Support function. Returns unique indices from infinite sequence.
     !!! May get stuck in an infinite loop,
     if required number of unique elements 
     more than total number of unique elements.
-}
takeUniqueIndices
    :: Eq a 
    => Int -- ^ Number of indices
    -> [a] -- ^ Sequence of random indices
    -> [a] 
takeUniqueIndices n list = takeUnique n list []
  where
    takeUnique 0 xs ys = ys
    takeUnique n (x:xs) ys
        | (elem x ys) = takeUnique n xs ys
        | otherwise   = takeUnique (pred n) xs (x:ys)


