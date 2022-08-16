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


