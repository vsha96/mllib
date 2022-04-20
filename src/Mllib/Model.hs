module Mllib.Model
    ( ModelClassification(..)
    ) where

import Mllib.Types
import Mllib.Metrics (Metric)

import Mllib.Classification.KNN
import Mllib.Classification.NearestCentroid



class ModelClassification model where
    predict :: model -> Metric -> [Vector R] -> [Int]



instance ModelClassification KNeighbors where
    predict = 
        Mllib.Classification.KNN.predict

instance ModelClassification NearestCentroid where
    predict = 
        Mllib.Classification.NearestCentroid.predict


