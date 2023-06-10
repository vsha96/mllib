module Mllib.Classification.KNN
    ( KNeighbors
    , KNeighborsParams(..)
    , fitKNeighbors
    , predict
    ) where

import Mllib.Types
import Mllib.Metrics (Metric)
import Mllib.Utils.Features (countFeatures)

import Data.List (maximumBy, sortBy)
import Data.Ord (comparing)

-- | KNeighbors configuration
data KNeighborsParams = KNeighborsParams
    { neighborNumber  :: Int      -- ^ Number of neighbors
    , distanceMetric  :: Metric   -- ^ Distance metric
    }

-- | KNearestNeighbors data type
data KNeighbors = KNeighbors
  { featureNumber  :: !Int        -- ^ Number of features
  , uniqueFeatures :: ![Int]      -- ^ Names of unique features
  , vectorNumber   :: !Int        -- ^ Vector quantity
  , vectors        :: ![Vector R] -- ^ List of vectors
  , labels         :: ![Int]      -- ^ Labels of vectors
  , params         :: KNeighborsParams

  -- TODO
  -- , weights        :: ???      -- ^ 
  }



{- | KNN fit function.
     Returns type 'KNeighbors' with extracted features inside.
-}
fitKNeighbors
    :: KNeighborsParams
    -> [Vector R] -- ^ List of vectors
    -> [Int]      -- ^ Labels of vectors
    -> KNeighbors
fitKNeighbors params vectors labels =
  let
    (featureNumber, uniqueFeatures) = countFeatures labels
    vectorNumber = length labels
  in  
    KNeighbors
      { featureNumber  = featureNumber
      , uniqueFeatures = uniqueFeatures
      , vectorNumber   = vectorNumber
      , vectors        = vectors
      , labels         = labels
      , params         = params
      }

{- | KNN predict function.
     Perform classification on a list vectors.
     Returns labels.
-}
predict
    :: KNeighbors  -- ^ NearestCentroid type after fitNearestCentroid
    -> [Vector R]  -- ^ List of vectors to classify
    -> [Int]
predict knn list =
  let 
    classes      = uniqueFeatures knn
    numOfVectors = vectorNumber knn
    k            = neighborNumber $ params knn
    distance     = distanceMetric $ params knn
    objects      = vectors knn
    tags         = labels knn
  in
    map (
        fst -- take label of class
        . maximumBy (comparing snd) -- find max number of neighbors 
                                    -- (!the last maximum will be selected 
                                    -- in equal situation)
        . countNeighbors classes -- count neighbors for each class
        . take k -- take k minimal distances (k nearest neighbors)
        . sortBy (comparing snd) -- sort distances
        . zip tags -- add labels
        . zipWith distance objects -- compute distance 
        . replicate numOfVectors -- prepare vector to calculate distance
                                 -- to each objected
    ) list

countNeighbors
    :: (Eq a, Ord b)
    => [a]        -- ^ classes
    -> [(a, b)]   -- ^ label and distance
    -> [(a, Int)]
countNeighbors classes list =
    [ (c, length (filter (\(label, d) -> label == c) list))
    | c <- classes]


