## Basic principles when implementing a model:  

**TL;DR**:
Open [the decision tree model](https://github.com/vsha96/mllib/blob/main/src/Mllib/Tree/Decision.hs) and write in a similar style

------------

The **models are divided** by tasks or by structure (see [src/Mllib](https://github.com/vsha96/mllib/tree/main/src/Mllib)).
There are also support modules.


The **data should be processed** as vectors from the package [hmatrix](https://hackage.haskell.org/package/hmatrix). Import of this data type and several functions were made in [Mllib/Types.hs](https://github.com/vsha96/mllib/blob/main/src/Mllib/Types.hs)

General code style is [here](https://kowainik.github.io/posts/2019-02-06-style-guide)

**After each item there is an example.**

The model should be written as follows:

1) Language extensions should be the first.
```haskell
{-# LANGUAGE LangExt #-}
```
2) Describe the model or give link to the description and write examples of usage.
```haskell
-- | Implementation of CART
-- There are different paramaters in `DecisionTreeParams`
-- For quick setup use `treeSetup` when fitting a tree
--
-- Example:
--    fitDecisionTree treeSetup x y
--
-- General example 1:
--   let
--     x = ...
--     y = ...
--     x_train = map vector x
--     x_test = map vector [...]
--     tree = fitDecisionTree treeSetup{...} x_train y
--   print $ tree
--   print $ predict tree x_test
```

3) Add notes for contributors. You can also mark some features or drawbacks of current implementation of the model.
```haskell
-- | Information for contributors
--   TODO list:
-- 1) fix classification when ...
...
-- 4) Add validation of paramaters
```

4) Definition of module with exports and imports
```haskell
module Mllib.Tree.Decision
    ( DecisionTreeParams(..)
    , DecisionTree
    , treeSetup
    , fitDecisionTree
    , predict
    ) where

import Mllib.Types
import Mllib.Utils.Features (...)

import Data.List (...)
import System.Random (...)
```

5) Data types
```haskell
-- | ============================
-- | ======== Data types ========
-- | ============================

-- | Decision tree parameters for setup
data DecisionTreeParams = DecisionTreeParams
    {
        ...
    }
```

6) Block of main functions
```haskell
-- | ================================
-- | ======== Main functions ========
-- | ================================

-- | Default parameters for decision tree
treeSetup :: DecisionTreeParams
treeSetup 
    = DecisionTreeParams
        {  
            ...
        }

-- | Decision tree fit function
fitDecisionTree 
    :: DecisionTreeParams -- ^ Tree parameters
    -> [Vector R]         -- ^ List of vectors
    -> [Int]              -- ^ Labels of vectors
    -> DecisionTree
fitDecisionTree params x y = ...
```

7) Internal functions that are used only in this model (not exported)
```haskell
-- | ====================================
-- | ======== Internal functions ========
-- | ====================================

-- Predict class label for one vector
predictElem
    :: DecisionTree     -- ^ Fitted decision tree
    -> Vector R         -- ^ Vector
    -> Int              -- ^ Predicted label for vector
predictElem tree x = ...
```

8) Support functions that may also be useful in other models (not exported)  
Try to find them in [Mllib/Utils](https://github.com/vsha96/mllib/tree/main/src/Mllib/Utils).
Otherwise, add them.
```haskell
-- | ===================================
-- | ======== Support functions ========
-- | ===================================

-- | Choose one element from the list randomly
chooseRandomElem
    :: [a]    -- ^ List
    -> StdGen -- ^ RandomGen
    -> a
chooseRandomElem xs rGen = ...
```

9) Unused functions  
Maybe you wrote and debugged a function that later turned out to be unnecessary, do not delete it, leave it here.
```haskell
-- | ==================================
-- | ======== Unused functions ========
-- | ==================================

...
```



## Testing

[See how to test in the HOW-TO-TEST.md](https://github.com/vsha96/mllib/blob/main/HOW_TO_TEST.md)



------------

#### If you see that these rules need to be improved, be free to open a ticket in the [issue tracker](https://github.com/vsha96/mllib/issues)



