## Mllib: Machine learning library in Haskell

The collection of machine learning algorithms that allows developers to stay within the Haskell language.

Documentation will be available soon!  

## Installation and run
- download and install [the Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
- download the latest version of this library
- try to use available models in [app/Main.hs](https://github.com/vsha96/mllib/blob/main/app/Main.hs)
- in the directory of package
`stack run`

## Available models

Clustering:
- `Mllib.Cluster.KMeans`

Classification:
- `Mllib.Classification.NearestCentroid`
- `Mllib.Classification.KNN`

Trees:
- `Mllib.Tree.Decision` *classification only*


#### Example of usage:
```haskell
-- file app/Main.hs
import Mllib.Types
import Mllib.Tree.Decision
...

main :: IO ()
main = do 
    ...
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

See more examples in [app/Main.hs]()


## Contributing

Any help is welcome! Please
make sure to read [the contributor guidelines](CONTRIBUTING.md) before
opening a new issue.
