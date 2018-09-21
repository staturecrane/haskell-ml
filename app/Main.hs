module Main where

import Data.List.Split
import Control.Monad.IO.Class
import Numeric.LinearAlgebra (                       Matrix, scale, R, (><))
import Numeric.LinearAlgebra.HMatrix                 (mul, sumElements)
import qualified System.Random.MWC                   as MWC
import qualified System.Random.MWC.Distributions     as MWC
import System.IO

type Gender = Double

meanMatrix :: Matrix R -> Int -> Double
meanMatrix inputs batchSize = (sumElements inputs) / (fromIntegral batchSize)

variance :: [Double] -> Double -> Double
variance inputs mean = sum [(x - mean)**2 | x <- inputs]

mult :: Matrix R -> Matrix R -> Matrix R
mult inputs weights = (inputs `mul` weights)

sigmoid :: Matrix R -> Matrix R
sigmoid inputs = 1.0 / (1.0 + exp (-1 * inputs))

forward :: Matrix R -> Matrix R -> Matrix R
forward inputs weights = sigmoid (mult inputs weights)

criterion :: Matrix R -> Matrix R -> Matrix R
criterion targets logits = (-1 * targets) * (log logits) - (1 - targets) * (log $ 1 - logits)

loss :: Matrix R -> Matrix R -> Int -> Double
loss targets logits batchSize = meanMatrix (criterion targets logits) batchSize

updateWeights :: Double -> Matrix R -> Matrix R -> Matrix R
updateWeights learningRate gradient weights = (weights - (learningRate `scale` gradient))

process :: [[[Char]]] -> [Gender]
process inputs = [getGender x | x <- inputs]

getGender :: [[Char]] -> Gender
getGender row = case (row !! 5) of
    "male"   -> 0.0
    "female" -> 1.0
    _ -> 0.0

readCSV :: String -> [[[Char]]]
readCSV file = [splitOn "," x | x <- (lines file)]

main :: IO ()
main = MWC.withSystemRandom $ \g -> do
    -- contents <- readFile "data/titanic/train.csv"
    -- print ( process (readCSV contents))
    p0 <- MWC.uniformR (-0.5 :: Double, 0.5 :: Double) g
    print p0
