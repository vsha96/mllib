module Mllib.Cluster.KMeansSpec where

import Test.Hspec

import Mllib.Types
import Mllib.Utils.Features (equalGroups)

import Mllib.Cluster.KMeans

spec :: Spec
spec = do

  let
    x = [[1, 2], [1, 3],[1, 0]
        ,[15, 4],[15, 0]]
    x_train = map vector x
    -- fitting model
    modelKMeans = fitKMeans kmeansSetup{clusterNumber = 2} x_train

  describe "KMeans" $ do
    it "performs clustering" $ do
      equalGroups (labels modelKMeans) [0,0,0,1,1] `shouldBe` True