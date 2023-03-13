module Mllib.Classification.NearestCentroidSpec where

import Test.Hspec

import Mllib.Types
import Mllib.Metrics

import Mllib.Classification.NearestCentroid

spec :: Spec
spec = do

  let
    x = [[0], [1], [2], [3]]
    y = [0, 0, 1, 1]
    x_train = map vector x
    -- fitting model
    params = NearestCentroidParams { distanceMetric=euclideanDistance }
    nc = fitNearestCentroid params x_train y
    -- tests
    x_test0 = map vector [[1.1]]
    x_test1 = map vector [[-10], [-20]]
    x_test2 = map vector [[10], [20]]

  describe "NearestCentroid" $ do
    it "performs classification" $ do
      (predict nc x_test0) `shouldBe` [0]
      (predict nc x_test1) `shouldBe` [0, 0]
      (predict nc x_test2) `shouldBe` [1, 1]
