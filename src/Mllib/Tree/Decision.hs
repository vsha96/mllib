{-# LANGUAGE NamedFieldPuns #-}

-- | Implementation of CART
-- There are different paramaters in `DecisionTreeParams`
-- For quick setup use `treeSetup` when fitting a tree
-- Example:
--    fitDecisionTree treeSetup x y
--
-- General example 1:
--   gen <- newStdGen
--   let
--     x =
--         [[1  , 1]
--         ,[1  , 2]
--         ,[3  , 1.5]
--         ,[1.5, 4]
--         ,[3  , 4]
--         ,[4  , 3.5]
--         ,[4 , 1]
--         ]
--     y =
--         [1
--         ,1
--         ,1
--         ,2
--         ,2
--         ,2
--         ,2
--         ]
--     x_train = map vector x
--     x_test = map vector
--         [[1, 2]
--         ,[4,2]
--         ]
--     tree = fitDecisionTree treeSetup{rGen=gen, randomSplitter=True, minSamplesLeaf=1} x_train y
--   print $ tree
--   print $ predict tree x_test
--
-- Example 2:
--    x = [[1], [2], [3], [4]]
--    y = [0, 1, 2, 3]
--    tree = fitDecisionTree treeSetup (map vector x) y
--    predict tree (map vector [[0], [1.6], [3.3], [10]])

module Mllib.Tree.Decision
    ( DecisionTreeParams(..)
    , DecisionTree
    , treeSetup
    , fitDecisionTree
    , predict
    ) where

import Mllib.Types
import Mllib.Utils.Features (countFeatures)

import Data.List (sortOn)
import System.Random (randomR, randomRs)


-- | ============================
-- | ======== Data types ========
-- | ============================

-- | Decision tree parameters for setup
data DecisionTreeParams = DecisionTreeParams
    { rGen            :: !StdGen  -- ^ Random generator
    , randomSplitter  :: !Bool    -- ^ If True, then chooses the best random split
                                  --   If False, then chooses the best split
    , maxDepth        :: !Int     -- ^ The maximum depth of the tree
    , minSamplesSplit :: !Int     -- ^ The minimum number of samples required
                                  --   to split a node
    , minSamplesLeaf  :: !Int     -- ^ The minimum number of samples required 
                                  --   to be at a leaf
    , maxFeatures     :: !Int     -- ^ The number of features to consider 
                                  --   when looking for the best split
    }
  deriving Show

-- | Decision tree data type
data DecisionTree =
    DecisionTreeNode
        { params         :: !DecisionTreeParams -- ^ Parameters for splitting  
        
        , currentDepth   :: !Int        -- ^ Current depth of the tree
        , vectorNumber   :: !Int        -- ^ Number of vectors
        , classesNumber  :: !Int        -- ^ Number of classes
        , classes        :: ![Int]      -- ^ Classes names

        , featureIndex   :: !Int        -- ^ Index of feature for 
                                        --   the node condition
        , nodeCondition  :: !Double     -- ^ Value for condition
        
        , subForest      :: !(DecisionTree, DecisionTree)
                                        -- ^ Left and right tree
        }
    | DecisionTreeLeaf
        { params         :: !DecisionTreeParams 
                                        -- ^ Parameters for splitting
        
        , currentDepth   :: !Int        -- ^ Current depth of the tree
        , vectorNumber   :: !Int        -- ^ Number of vectors
        , classesNumber  :: !Int        -- ^ Number of classes
        , classes        :: ![Int]      -- ^ Classes names
        
        , classLabel     :: !Int        -- ^ Result class
        
        , vectors        :: [Vector R]  -- ^ List of vectors
        , labels         :: [Int]       -- ^ Labels of vectors
        }


-- | ================================
-- | ======== Main functions ========
-- | ================================

-- | Default parameters for decision tree
treeSetup :: DecisionTreeParams
treeSetup 
    = DecisionTreeParams
        { rGen           = mkStdGen 42
        , randomSplitter = False
        , maxDepth       = maxBound
        , minSamplesSplit= 2
        , minSamplesLeaf = 1
        , maxFeatures    = -1
        }

-- | Decision tree fit function
fitDecisionTree 
    :: DecisionTreeParams -- ^ Tree parameters
    -> [Vector R]         -- ^ List of vectors
    -> [Int]              -- ^ Labels of vectors
    -> DecisionTree
fitDecisionTree params x y 
    = buildDecisionTree 
        (validateParams params dimension) 
        1    {-current depth-} 
        (-1) {-feature index-} 
        x 
        y
  where
    dimension = size $ head x

-- Predict class labels for vectors
predict
    :: DecisionTree     -- ^ Fitted decision tree
    -> [Vector R]       -- ^ Vectors
    -> [Int]            -- ^ Predicted labels for vectors
predict tree = map (predictElem tree)



-- | ====================================
-- | ======== Internal functions ========
-- | ====================================

-- Predict class label for one vector
predictElem
    :: DecisionTree     -- ^ Fitted decision tree
    -> Vector R         -- ^ Vector
    -> Int              -- ^ Predicted label for vector
predictElem tree x = case tree of
    DecisionTreeLeaf {classLabel} 
        -> classLabel
    DecisionTreeNode {featureIndex, nodeCondition, subForest = (lTree, rTree)} 
        | xi < nodeCondition -> predictElem lTree x
        | otherwise          -> predictElem rTree x
      where
        xi = (toList x) !! featureIndex



-- | Impurity criteria
-- !working with proportions of classes in the group
type Criteria = [Double] -> Double

-- | Gini's impurity
gini
    :: [Double] -- ^ Proportions of classes
    -> Double
gini props = 1 - sum (map (**2) props)

-- | Number of occurrences of the element in the list
countOccurrences
    :: Eq a
    => a   -- ^ element
    -> [(b,a)] -- ^ list
    -> Int
countOccurrences x = length . filter (\y -> x == snd y)

-- | Proportions of given group according to class names
-- Returns list of proportions
proportions
    :: [Int]      -- ^ class names
    -> [(a, Int)] -- ^ a group where element is (vector, label)
    -> [Double]
proportions classes group = 
    [fromIntegral (countOccurrences cl group) / totalSize | cl <- classes]
  where
    totalSize = fromIntegral $ length group

-- | Compute impurity of the split according to the criteria
-- ! Unique class names are needed
computeImpurity
    :: Criteria -- ^ impurity criteria
    -> [Int]    -- ^ class names
    -> ([(a, Int)] , [(a, Int)]) -- ^ split, tuple of lists or groups where element is (vector, label)
    -> Double
computeImpurity criteria uniqueClasses (lgroup, rgroup) = 
  let
    totalSize = fromIntegral $ length lgroup + length rgroup
    lGroupWeight = fromIntegral (length lgroup) / totalSize
    rGroupWeight = fromIntegral (length rgroup) / totalSize
  in
    ((criteria . proportions uniqueClasses) lgroup) * lGroupWeight 
        + ((criteria . proportions uniqueClasses) rgroup) * rGroupWeight



-- | Possible splits with decision tree params
-- Returns list of tuples with left and right groups respectively
possibleSplitsWithParams
    :: DecisionTreeParams
    -> [a]         -- ^ given list
    -> [([a],[a])]
possibleSplitsWithParams params list = 
  let
    minLen = minSamplesLeaf params
    len = length list
  in
    [ splitAt i list 
        | i <- [1..(len - 1)]
        , i >= minLen
        , (len - i) >= minLen
    ]

-- | Find the best split by feature at index according to tree params
-- It sorts vectors with labels before computing impurity
-- Returns tuple of impurity and split
findBestSplitByFeatureWithParams
    :: Int   -- ^ index of feature
    -> DecisionTreeParams -- ^ tree params
    -> [Int] -- ^ class names
    -> [Vector R]   -- ^ vectors
    -> [Int] -- ^ labels
    -> (Double    -- ^ gini's impurity
       , Int      -- ^ index of feature by which vectors were sorted
       ,([(Vector R, Int)] , [(Vector R, Int)]) 
                  -- ^ split or two groups where element is (vector, label)
       ) 
findBestSplitByFeatureWithParams index params uniqueClasses x y =
  let
    xLists = map toList x -- transform vectors to lists
    -- TODO: look in HMatrix package:
    --   can we avoid transformation via `atIndex` or `!` ?
    zLists = sortOn ((!! index) . fst) (zip xLists y) -- packing transformed vecs with labels and sort
    zVecs  = map (\ (x, label) -> (fromList x, label)) zLists -- transform sorted lists to vectors
  in
    (\(imp, split) -> (imp, index, split)) $ -- add information about index
    head $ -- taking minimum as (minImpurityValue, split)
    -- TODO: if there are several equal values then choose random
    sortOn fst $ -- sort by impurity value
    zip -- packing impurity value and split
      (map
        (computeImpurity gini uniqueClasses) 
        (possibleSplitsWithParams params zVecs))
      (possibleSplitsWithParams params zVecs)

-- | Get value from split for node condition of the tree
computeBorder
    :: (Double    -- ^ gini's impurity
       , Int      -- ^ index of feature by which vectors were sorted
       ,([(Vector R, Int)] , [(Vector R, Int)]) -- ^ split or two groups where element is (vector, label)
       ) 
    -> R -- ^ border for predicate
computeBorder (_, index, (lgroup, rgroup)) = (lastLeftValue + firstRightValue) / 2
  where
    lastLeftValue   = (!!index) $ toList $ fst $ last lgroup
    firstRightValue = (!!index) $ toList $ fst $ head rgroup

-- | Make splits for spicific features of vectors
-- according to decision tree params
makeBestSplitsForFeatures
    :: DecisionTreeParams -- ^ tree parameters
    -> [Int]      -- ^ class names
    -> [Int]      -- ^ list of feature indices
    -> [Vector R] -- ^ vectors
    -> [Int]      -- ^ labels
    -> [(Double   -- ^ gini's impurity
       , Int      -- ^ index of feature by which vectors were sorted
       ,([(Vector R, Int)] , [(Vector R, Int)]) -- ^ split or two groups where element is (vector, label)
       )] -- ^ list of splits for each feature
makeBestSplitsForFeatures params uniqueClasses indices x y  
    -- TODO: if there are several equal values then choose random
    = sortOn 
        (\(imp, index, split) -> imp) -- sort by impurity value
        [findBestSplitByFeatureWithParams i params uniqueClasses x y | i <- indices]



-- | Get a classification label by majority vote
mode
    :: [Int] -- ^ Names of classes
    -> [Int] -- ^ Labels of vectors
    -> Int
mode classes y = 
    fst $ last $ -- take class label of maximum
    -- TODO: solve problem with several maximums
    sortOn snd -- sort by quantity
    [(cl, length (filter (==cl) y)) | cl <- classes] -- take class label and make tuple with its quantity



-- | Validation of decision tree parameters
-- Params must make sense
validateParams
    :: DecisionTreeParams -- ^ Tree parameters
    -> Int                -- ^ Dimension of vectors
    -> DecisionTreeParams
validateParams params dimension
    -- TODO: add more checks for errors
    | maxFeatures params == (-1) 
    -- this means that maxFeatures is undefined
    -- then take the square root of dimension
        = params{ maxFeatures=(truncate (sqrt (fromIntegral dimension))) }
    | maxFeatures params < 1
        = error "ERR: invalid params"
    | otherwise
        = params

-- | Check stop conditions of decision tree
checkStopCond
    :: DecisionTreeParams -- ^ Tree parameters
    -> Int                -- ^ Current depth
    -> [Vector R]         -- ^ List of vectors
    -> [Int]              -- ^ Labels of vectors
    -> Bool
checkStopCond params curDepth x y = 
    (maxDepth params) == curDepth
    || classesNumber == 1
    || lenX == 1
    || lenX < (minSamplesSplit params)
    || lenX < (2 * minSamplesLeaf params)
  where
    (classesNumber, classes) = countFeatures y
    lenX = length x

-- | Generate a list of features for splits
-- according to the index of previous split
generateFeatures
    :: DecisionTreeParams -- ^ tree parameters
    -> Int        -- ^ feature index of previous split
    -> Int        -- ^ dimension of vectors
    -> [Int]      -- ^ labels
generateFeatures params prevIndex dimension 
    | prevIndex == -1 
        -- then it is the first split, we can consider all features
        = takeUniqueIndices
            (maxFeatures params)
            (randomRs (0, dimension - 1) (rGen params))
    | dimension == 1
        -- then split by the same feature index, it is 0
        = [0]
    | maxFeatures params == dimension 
        -- then take all indices except the previous
        = filter (/= prevIndex) $ [0..(dimension-1)]
    | maxFeatures params <  dimension
        -- then we can exclude previous index
        = takeUniqueExceptElem -- take elements
            (maxFeatures params) -- number of elements
            (randomRs (0, dimension - 1) (rGen params)) -- random sequence
            prevIndex -- except this element
    | otherwise
        = error "ERR: generateFeatures: maxFeatures > dimension of vector"

-- | Make split according to decision tree params and feature index of previous split
makeSplit
    :: DecisionTreeParams -- ^ tree parameters
    -> Int        -- ^ feature index of previous split
    -> [Int]      -- ^ class names
    -> [Vector R] -- ^ vectors
    -> [Int]      -- ^ labels
    -> (Double    -- ^ gini's impurity
       , Int      -- ^ index of feature by which vectors were sorted
       ,([(Vector R, Int)] , [(Vector R, Int)]) -- ^ split or two groups where element is (vector, label)
       ) -- ^ split
makeSplit params prevIndex uniqueClasses x y 
    | randomSplitter params 
        = chooseRandomElem 
            (makeBestSplitsForFeatures params uniqueClasses features x y) 
            (rGen params)
    | otherwise 
        = head
            (makeBestSplitsForFeatures params uniqueClasses features x y)
  where
    gen      = rGen params
    dimension = size $ head x
    features = generateFeatures params prevIndex dimension

-- | Main internal function from `fitDecisionTree`
-- Builds decision tree node or leaf according to stop conditions
buildDecisionTree
    :: DecisionTreeParams -- ^ Tree parameters
    -> Int                -- ^ Current depth
    -> Int                -- ^ Feature index of previous split
    -> [Vector R]         -- ^ List of vectors
    -> [Int]              -- ^ Labels of vectors
    -> DecisionTree
buildDecisionTree params curDepth prevIndex x y 
    | checkStopCond params curDepth x y = 
        DecisionTreeLeaf
            { params = params

            , currentDepth = curDepth
            , vectorNumber = length x
            , classesNumber = classesNumber
            , classes = classes

            , classLabel = mode classes y

            , vectors = x
            , labels = y
            }
    | otherwise = 
        DecisionTreeNode
            { params = params

            , currentDepth = curDepth
            , vectorNumber = length x
            , classesNumber = classesNumber
            , classes = classes

            , featureIndex = index
            , nodeCondition = computeBorder split

            , subForest = 
                (buildDecisionTree params (succ curDepth) index xLeft  yLeft
                ,buildDecisionTree params (succ curDepth) index xRight yRight)
            }
  where
    (classesNumber, classes) = countFeatures y
    split = makeSplit params prevIndex classes x y
    (_, index, (lGroup, rGroup)) = split
    (xLeft, yLeft) = unzip lGroup
    (xRight, yRight) = unzip rGroup



-- | ===================================
-- | ======== Support functions ========
-- | ===================================

-- | Choose one element from the list randomly
chooseRandomElem
    :: [a]    -- ^ List
    -> StdGen -- ^ RandomGen
    -> a
chooseRandomElem xs rGen 
    | len == 1  = xs !! 0
    | len >  1  = xs !! rIndex
    | otherwise = error "ERR: chooseRandomElem: null list"
  where
    len = length xs
    (rIndex, _) = randomR (0, (len-1)) rGen

-- | Support function. Returns unique indices from infinite sequence.
-- !!! May get stuck in an infinite loop,
-- if required number of unique elements 
-- more than total number of unique elements.
-- TODO: make it safer
takeUniqueIndices
    :: Eq a 
    => Int -- ^ Number of indices
    -> [a] -- ^ Sequence of random indices
    -> [a] 
takeUniqueIndices n list = takeUnique n list []
  where
    takeUnique 0 xs ys = ys
    takeUnique n (x:xs) ys
        | (elem x ys) = takeUnique n xs ys
        | otherwise   = takeUnique (pred n) xs (x:ys)

-- | Support function. Returns unique indices from infinite sequence except one elem. 
-- !!! May get stuck in an infinite loop,
-- if required number of unique elements 
-- more than total number of unique elements.
-- TODO: make it safer
takeUniqueExceptElem
    :: Eq a 
    => Int -- ^ Number of indices
    -> [a] -- ^ Sequence of random indices
    -> a   -- ^ Except this element
    -> [a] 
takeUniqueExceptElem n list ex = takeUnique n list ex []
  where
    takeUnique 0 xs ex ys = ys
    takeUnique n (x:xs) ex ys
        | (elem x ys) || (x == ex) 
            = takeUnique n xs ex ys
        | otherwise
            = takeUnique (pred n) xs ex (x:ys)



instance Show DecisionTree where
    show 
      (DecisionTreeLeaf 
        params 
        currentDepth 
        vectorNumber 
        classesNumber 
        classes 
        classLabel 
        vectors 
        labels) 
            = "#TreeLeaf curDepth: " ++ show currentDepth ++ 
                " classLabel: " ++ show classLabel ++ 
                " vectors: " ++ show vectors ++ 
                " labels: " ++ show labels
    show 
      (DecisionTreeNode
        params 
        currentDepth 
        vectorNumber
        classesNumber 
        classes 
        featureIndex 
        nodeCondition 
        (lTree, rTree)) 
            = "#TreeNode curDepth: " ++ show currentDepth ++ 
                " predicate: " ++ "x" ++ show featureIndex ++ " < "++ show nodeCondition ++
                "\n" ++ concat (replicate (currentDepth+1) "  ") ++ "#Left" ++ show lTree ++
                "\n" ++ concat (replicate (currentDepth+1) "  ") ++ "#Right" ++ show rTree






-- | ==================================
-- | ======== Unused functions ========
-- | ==================================

-- These are in the dev_notebooks: Tree_Decision.ipynb


