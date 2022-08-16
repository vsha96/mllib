module Mllib.Utils.Features
    ( countFeatures
    ) where

-- | Generalized function based on 'Eq' classes.
-- Returns number and labels of features seen.
countFeatures
    :: (Eq a)
    => [a]        -- ^ Labels
    -> (Int, [a]) -- ^ Number and labels of unique features
countFeatures labels = counting labels []

-- | countFeatures help function
counting
    :: (Eq a)
    => [a]        -- ^ Labels
    -> [a]        -- ^ List of unique labels
    -> (Int, [a]) -- ^ Number and labels of unique features
counting []     ys = (length ys, reverse ys)
counting (x:xs) ys
    | elem x ys = counting xs ys
    | otherwise = counting xs (x:ys)


-- Take unique elements from the list
takeUniqueElems
    :: Eq a
    => [a] -- list
    -> [a] -- list of unique elements
takeUniqueElems = _takeUniqueElems []

_takeUniqueElems
    :: Eq a
    => [a] -- acc list of unique elements
    -> [a] -- list
    -> [a] -- list of unique elems
_takeUniqueElems acc [] = acc
_takeUniqueElems acc (x:xs)
    | elem x acc = _takeUniqueElems acc xs
    | otherwise  = _takeUniqueElems (x:acc) xs


-- Check the lists divided into the same groups
-- e.g. 
--  [0,0,1,0] and [5,5,0,5] have the same groups
--  [1,2,3] and [0,1,0] don't have, but [1,2,3] and [0,1,5] have
equalGroups
    :: Eq a
    => [a]
    -> [a]
    -> Bool
equalGroups [] [] = True
equalGroups xs ys 
    | (length xs == length ys) = 
      let
        uniqueElemsX = takeUniqueElems xs
        uniqueElemsY = takeUniqueElems ys
        lenUniqueX   = length uniqueElemsX
        lenUniqueY   = length uniqueElemsY
        groupsX      = zipWith elemIndices uniqueElemsX (replicate lenUniqueX xs)
        groupsY      = zipWith elemIndices uniqueElemsY (replicate lenUniqueY ys)
      in
        groupsX == groupsY
    | otherwise = False

