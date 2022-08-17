{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
module Main where

import Test.Hspec

import qualified Mllib.Classification.KNNSpec
import qualified Mllib.Classification.NearestCentroidSpec
import qualified Mllib.Cluster.KMeansSpec
import qualified Mllib.Tree.DecisionSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Mllib.Classification" Mllib.Classification.KNNSpec.spec
  describe "Mllib.Classification" Mllib.Classification.NearestCentroidSpec.spec
  describe "Mllib.Cluster"        Mllib.Cluster.KMeansSpec.spec
  describe "Mllib.Tree"           Mllib.Tree.DecisionSpec.spec

