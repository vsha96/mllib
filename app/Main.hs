module Main where

import System.Random

import Mllib.Types
import Mllib.Metrics



-- | Available models
-- remove comments from one (to avoid conflicts) desired model 
-- in the import block below and in `main` function

import Mllib.Tree.Decision as DT
-- import Mllib.Classification.KNN as KNN
-- import Mllib.Classification.NearestCentroid as NC
-- import Mllib.Cluster.KMeans as KM

main :: IO ()
main = do 

  gen <- newStdGen
  putStrLn "\nSTART"

  -- | Example of Decision Tree
  let
      x = [[1], [2], [3], [4]]
      y = [0, 0, 2, 2]
      x_train = map vector x
      x_test  = map vector [[0], [1.8], [3.3], [10]]
      modelDTree = fitDecisionTree treeSetup{rGen=gen} x_train y
  putStr "Model: "
  print $ modelDTree
  putStr $ "Predict " ++ (show x_test) ++ ": "
  print $ predict modelDTree x_test

  -- | Example of KNN
  {-
  let
    x = [[0], [1], [2], [3]]
    y = [0, 0, 1, 1]
    x_train = map vector x
    x_test = map vector [[1.1]]
    modelKNN = fitKNeighbors 3 x_train y
  putStr "Model: "
  print $ modelKNN
  putStr $ "Predict " ++ (show x_test) ++ ": "
  print $ predict modelKNN euclideanDistance x_test
  -}

  -- | Example of NearestCentroid
  {-
  let
    x = [[0], [1], [2], [3]]
    y = [0, 0, 1, 1]
    x_train = map vector x
    x_test = map vector [[1.1], [-10], [2.7]]
    modelNC = fitNearestCentroid x_train y
  putStr "Model: "
  print $ modelNC
  putStr $ "Predict " ++ (show x_test) ++ ": "
  print $ predict modelNC euclideanDistance x_test
  -}

  -- | Example of KMeans
  {-
  let
    x = [[1, 2]
        ,[1, 4]
        ,[1, 0]
        ,[10, 2]
        ,[10, 4]
        ,[10, 0]]
    x_train = map vector x
    modelKMeans = fitKMeans kmeansSetup{rGen=gen, clusterNumber = 2} x_train
  putStr "Model: "
  print $ modelKMeans
  putStr "Clusterization: "
  print $ labels $ modelKMeans
  -}

  putStrLn "END"


