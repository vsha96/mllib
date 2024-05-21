module Mllib.Metrics
    ( Metric
    , euclideanDistance
    , minkowskiDistance
    , chebyshevDistance
    , manhattanDistance
    ) where

import Mllib.Types

import Numeric.LinearAlgebra (cmap, sumElements, maxElement)

-- TODO: add instance show? but how?
-- maybe change Metric to data (enum-like), so that we can provide additional context
type Metric = (Vector R) -> (Vector R) -> R 

euclideanDistance :: Metric
euclideanDistance v1 v2 = (sumElements (cmap (**2) (v1 - v2))) ** (1/2)

minkowskiDistance :: Int -> Metric
minkowskiDistance p v1 v2 = (sumElements (cmap (**pc) (v1 - v2))) ** (1 / pc)
  where
    pc = fromIntegral p

chebyshevDistance :: Metric
chebyshevDistance v1 v2 = maxElement $ cmap abs (v1 - v2)

manhattanDistance :: Metric 
manhattanDistance v1 v2 = sumElements $ cmap abs (v1 - v2)

-- TODO test all above

-- TODO more metrics
-- -- Hamming
-- -- cos(x,z)
-- -- Canberra distance
-- -- ? Levenstein


