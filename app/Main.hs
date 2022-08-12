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
  putStr $ "Predict " ++ (show x_test) ++ ": "
  print $ predict knn euclideanDistance x_test

  -- | Example of KMeans usage
  -- let
  --   x = [[1, 2]
  --       ,[1, 4]
  --       ,[1, 0]
  --       ,[10, 2]
  --       ,[10, 4]
  --       ,[10, 0]]
  --   x_train = map vector x
  --   model = fitKMeans kmeansSetup{rGen=gen, clusterNumber = 2} x_train
  -- putStr "Model: "
  -- print $ model
  -- putStr "Prediction: "
  -- print $ labels $ model

  putStrLn "END"





