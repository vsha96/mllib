-- | Module for CSV files processing
-- !!! serious improvements are required
-- use at your own risk

module Mllib.Utils.CSV
    ( extractDataFromCSV
    , saveLabelsToCSV
    , convertToVector
    , toDouble
    , toInt
    ) where

import Mllib.Types



-- TODO code style 

import Text.CSV as TC
import Text.Parsec.Error (ParseError)
import Data.Either
import Data.Csv
import qualified Data.ByteString.Lazy as BSL

instance (ToRecord Int) where
    toRecord x = record [toField x]

-- TODO example of using (see examples of function in code style)
--    you need to define handle error and how you process data by your own
-- handleError csv = error "ERR: Mllib.CSV : handleError"
-- process = convertToVector . init . tail
extractDataFromCSV :: (ParseError -> b) -> ([TC.Record] -> b) -> FilePath -> IO b
extractDataFromCSV handleError process fileName = do
    input <- readFile fileName
    return $ either handleError process (parseCSV fileName input)

saveLabelsToCSV :: FilePath -> [Int] -> IO ()
saveLabelsToCSV fileName list = do
    BSL.writeFile fileName $ encode ((-1) : list)

convertToVector :: [TC.Record] -> [Vector R]
convertToVector = map (fromList) . map (map toDouble)

toDouble :: String -> Double
toDouble = read

toInt :: String -> Int
toInt = read

-- TODO make data extraction by header
-- or a function which returns [(String {- header -}, String)]
--      lookup in this list
--      and after this we can transform data

-- | Place code below in Main.hs to extract data from csv
{-
-- | For csv processing if use Mllib.Utils.CSV
handleError csv = error "ERR: Mllib.Utils.CSV : parse error"
process = convertToVector . init . tail
-}