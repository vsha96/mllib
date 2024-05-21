module Mllib.Classification.KNNSpec where

import Test.Hspec

import Mllib.Types
import Mllib.Metrics

import Mllib.Classification.KNN

spec :: Spec
spec = do

  let
    x = [[0], [1], [2], [3]]
    y = [0, 0, 1, 1]
    x_train = map vector x
    parameters = KNeighborsParams {neighborNumber=3, distanceMetric=euclideanDistance}
    -- fitting model
    knn = fitKNeighbors parameters x_train y
    -- tests
    x_test0 = [vector [1.1]]
    x_test1 = map vector [[-10], [-20]]
    x_test2 = map vector [[10], [20]]

  describe "KNN" $ do
    it "performs classification" $ do
      (predict knn x_test0) `shouldBe` [0]
      (predict knn x_test1) `shouldBe` [0, 0]
      (predict knn x_test2) `shouldBe` [1, 1]
