## Mllib: Machine learning library in Haskell

The collection of machine learning algorithms that allows developers to stay within the Haskell language.

Documentation will be available soon!  

## Installation and run
1. Download and install [the Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)  
`curl -sSL https://get.haskellstack.org/ | sh`
2. Clone the repository to download the latest version of this library  
`git clone git@github.com:vsha96/mllib.git`
3. First things to try out:
    - try to use available models in [app/Main.hs](https://github.com/vsha96/mllib/blob/main/app/Main.hs)
    - in the directory of package `stack run`

See [troubleshooting installation issues](https://github.com/vsha96/mllib/blob/main/docs/INSTALLATION_TROUBLESHOOTING.md)

## Available models

The models are divided by tasks or by structure (see [src/Mllib](https://github.com/vsha96/mllib/tree/main/src/Mllib)). There are also support modules.

Clustering:
* [`Mllib.Cluster.KMeans`](https://github.com/vsha96/mllib/blob/main/src/Mllib/Cluster/KMeans.hs)

Classification:
* [`Mllib.Classification.NearestCentroid`](https://github.com/vsha96/mllib/blob/main/src/Mllib/Classification/NearestCentroid.hs)
* [`Mllib.Classification.KNN`](https://github.com/vsha96/mllib/blob/main/src/Mllib/Classification/KNN.hs)

Trees:
* [`Mllib.Tree.Decision`](https://github.com/vsha96/mllib/blob/main/src/Mllib/Tree/Decision.hs) (classification only)

#### Example of usage:
```haskell
-- file app/Main.hs
import Mllib.Types
import Mllib.Tree.Decision

main :: IO ()
main = do 
  let
    x = [[1], [2], [3], [4]]
    y = [0, 0, 2, 2]
    x_train = map vector x
    x_test  = map vector [[0], [1.8], [3.3], [10]]
    modelDTree = fitDecisionTree treeSetup x_train y
  putStr "Predict: "
  print $ predict modelDTree x_test
```
Output:  
```Predict: [0,0,2,2]```

#### See more examples in [app/Main.hs](https://github.com/vsha96/mllib/blob/main/app/Main.hs#L10)


## Contributing

Any help is welcome! Please
make sure to read [the contributor guidelines](CONTRIBUTING.md) before
opening a new issue.
