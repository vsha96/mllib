## Mllib: Machine learning library in Haskell

The collection of machine learning algorithms that allows developers to stay within the Haskell language.

Documentation will be available soon!  

## Installation and run
- download and install [the Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
- download the latest version of **Mllib**
- try to use available models in **app/Main.hs**
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

## Contributing

Any help is welcome! Please
make sure to read [the contributor guidelines](CONTRIBUTING.md) before
opening a new issue.
