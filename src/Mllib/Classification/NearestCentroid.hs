module Mllib.Classification.NearestCentroid
    ( NearestCentroid
    , fitNearestCentroid
    , predict
    ) where

import Mllib.Types
import Mllib.Metrics (Metric)
import Mllib.Utils.Features (countFeatures)

import Data.List (minimumBy)
import Data.Foldable (foldMap)
import Data.Monoid (Sum(..))
import Data.Ord (comparing)



-- | NearestCentroid data type
data NearestCentroid = NearestCentroid
  { featureNumber  :: !Int        -- ^ Number of features
  , uniqueFeatures :: ![Int]      -- ^ List of unique classes (in order of the centroids)
  , clusterCenters :: ![Vector R] -- ^ Coordinates of centroids
  -- , metric         :: !Metric  -- ^ Metric
  }
  deriving Show



{- | NearestCentroid fit function.
     Returns type 'NearestCentroid' with centroids inside.
-}
fitNearestCentroid
    :: [Vector R] -- ^ List of vectors of double
    -> [Int]      -- ^ Labels
    -> NearestCentroid
fitNearestCentroid list labels = NearestCentroid featureNumber uniqueFeatures centroids
  where
    (featureNumber, uniqueFeatures) = countFeatures labels
    centroids = computeCentroids list labels uniqueFeatures

{- | NearestCentroid predict function.
     Perform classification on a list vectors.
     Returns labels.
-}
predict
    :: NearestCentroid -- ^ NearestCentroid type after fitNearestCentroid
    -> Metric          -- ^ Metric from Mllib.Metrics
    -> [Vector R]      -- ^ List of vectors to classify
    -> [Int]
predict nc distance list = 
    map (
      fst . -- take label of class
      minimumBy (comparing snd) . -- find min distance from centroid
      zip classes . -- add centroid label
      zipWith distance centroidVectors . -- compute distance to centroids
      replicate numOfClust -- prepare vector to calculate distance to centroids
    ) list
  where
    centroidVectors = clusterCenters nc
    numOfClust      = featureNumber nc
    classes         = uniqueFeatures nc

-- | Compute centroids of clusters for each class.
--   Returns centroids.
computeCentroids
    :: Eq a
    => [Vector R] -- ^ Vectors
    -> [a]        -- ^ Labels
    -> [a]        -- ^ Labels of unique features
    -> [Vector R] -- ^ Centers of clusters
computeCentroids list indices uniqueFeatures = 
    map (\(s, n) -> getSum s / getSum n) -- mean from the trick below
    [ foldMap (\x -> (Sum (snd x), Sum 1)) -- trick to count as (SumOfVecs, NumberOfVecs)
    $ filter (\tuple -> fst tuple == i) indexAndVector -- gather vectors by an index
    | i <- uniqueFeatures ] -- compute for each cluster
  where
    indexAndVector = zipWith (\x y -> (x, y)) indices list


