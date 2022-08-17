module Mllib.Tree.DecisionSpec where

import Test.Hspec

import Mllib.Types

import Mllib.Tree.Decision

spec :: Spec
spec = do

  let
    x = [[1], [2], [3], [4]]
    y = [0, 1, 2, 3]
    -- fitting model
    tree = fitDecisionTree treeSetup (map vector x) y
    -- tests
    x_test0 = map vector [[0]]
    x_test1 = map vector [[1.6], [3.3]]
    x_test2 = map vector [[10]]

  describe "Decision Tree" $ do
    it "performs classification" $ do
      (predict tree x_test0) `shouldBe` [0]
      (predict tree x_test1) `shouldBe` [1, 2]
      (predict tree x_test2) `shouldBe` [3]
