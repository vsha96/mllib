module Mllib.Classification.KNN
    ( KNeighbors
    , fitKNeighbors
    , predict
    ) where

import Mllib.Types
import Mllib.Metrics (Metric)
import Mllib.Utils.Features (countFeatures)

import Data.List (maximumBy, sortBy)
import Data.Ord (comparing)



-- | KNearestNeighbors data type
data KNeighbors = KNeighbors
  { featureNumber  :: !Int        -- ^ Number of features
  , uniqueFeatures :: ![Int]      -- ^ Names of unique features
  , vectorNumber   :: !Int        -- ^ Vector quantity
  , neighborNumber :: !Int        -- ^ Number of neighbors
  , vectors        :: ![Vector R] -- ^ List of vectors
  , labels         :: ![Int]      -- ^ Labels of vectors

  -- TODO
  -- , weights        :: ???      -- ^ 
  -- , metric         :: !Metric  -- ^ Metric
  }
  deriving Show



{- | KNN fit function.
     Returns type 'KNeighbors' with extracted features inside.
-}
fitKNeighbors
    :: Int        -- ^ Number of neighbors
    -> [Vector R] -- ^ List of vectors
    -> [Int]      -- ^ Labels of vectors
    -> KNeighbors
fitKNeighbors k vectors labels =
  let
    (featureNumber, uniqueFeatures) = countFeatures labels
    vectorNumber = length labels
  in  
    KNeighbors
      { featureNumber  = featureNumber
      , uniqueFeatures = uniqueFeatures
      , vectorNumber   = vectorNumber
      , neighborNumber = k
      , vectors        = vectors
      , labels         = labels
      }

{- | KNN predict function.
     Perform classification on a list vectors.
     Returns labels.
-}
predict
    :: KNeighbors  -- ^ NearestCentroid type after fitNearestCentroid
    -> Metric      -- ^ Metric from Mllib.Metrics
    -> [Vector R]  -- ^ List of vectors to classify
    -> [Int]
predict knn distance list =
  let 
    classes      = uniqueFeatures knn
    numOfVectors = vectorNumber knn
    k            = neighborNumber knn
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


