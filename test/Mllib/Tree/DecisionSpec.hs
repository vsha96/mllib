module Mllib.Tree.DecisionSpec where

import Test.Hspec

import Mllib.Types

import Mllib.Tree.Decision
import Control.Exception (evaluate)

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
    it "should fail on mismatching sizes" $ do
      let bad_x = map vector [[1, 2], [3], [4], [5]]
      evaluate (fitDecisionTree treeSetup bad_x y) `shouldThrow` errorCall "ERR: Input vectors have mismatching size"
      evaluate (predict tree bad_x) `shouldThrow` errorCall "ERR: Input vectors have mismatching size"
      let bad_y = tail y
      evaluate (fitDecisionTree treeSetup (map vector x) bad_y) `shouldThrow` errorCall "ERR: Samples and labels lists do not match in length"
