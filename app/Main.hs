module Main where

import System.Random

import Mllib.Types
import Mllib.Metrics
import Mllib.Utils.CSV


-- | Available models
import Mllib.Classification.KNN
-- import Mllib.Cluster.KMeans
-- import Mllib.Classification.NearestCentroid as NC
-- import Mllib.Tree.Decision


-- | For csv processing if use Mllib.Utils.CSV
handleError csv = error "ERR: Mllib.Utils.CSV : parse error"
process = convertToVector . init . tail

main :: IO ()
main = do 
  gen <- newStdGen
  putStrLn "\nSTART"

  let
    x = [[0], [1], [2], [3]]
    y = [0, 0, 1, 1]
    x_train = map vector x
    x_test = map vector [[1.1]]

    knn = fitKNeighbors 3 x_train y
  
  putStr "Model: "
  print $ knn
  putStr "Prediction: "
  print $ predict knn euclideanDistance x_test

  putStrLn "END"


