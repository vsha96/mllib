module Mllib.Types
    ( module Reexport
    ) where

import Data.Tree as Reexport (Tree, Forest)

import Numeric.LinearAlgebra.Data    as Reexport (fromList, vector)
import Numeric.LinearAlgebra.Data    as Reexport (Vector, R)

-- TODO: replace it
import Numeric.LinearAlgebra.HMatrix as Reexport (norm_2)

import System.Random as Reexport (StdGen, mkStdGen)



-- ======== ANOTHER WAY TO IMPORT ========

{-
module Mllib.Types
    ( norm_2
    , fromList
    , Vector
    , R
    ) where

import Numeric.LinearAlgebra.HMatrix (norm_2)
import Numeric.LinearAlgebra.Data (fromList) 
import Numeric.LinearAlgebra.Data (Vector, R)
-}


