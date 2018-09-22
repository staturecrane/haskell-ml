module Main where

import Data.List.Split
import Control.Monad.State
import Numeric.LinearAlgebra                         (col, disp, Matrix, R, rand, scale, toLists, tr, (><))
import Numeric.LinearAlgebra.HMatrix                 (mul, sumElements)
import System.IO

type Gender = Double
type Survived = Double

meanMatrix :: Matrix R -> Int -> Double
meanMatrix inputs batchSize = (sumElements inputs) / (fromIntegral batchSize)

variance :: [Double] -> Double -> Double
variance inputs mean = sum [(x - mean)**2 | x <- inputs]

mult :: Matrix R -> Matrix R -> Matrix R
mult inputs weights = (inputs * weights)

sigmoid :: Matrix R -> Matrix R
sigmoid inputs = 1.0 / (1.0 + exp (-1 * inputs))

forward :: Matrix R -> Matrix R -> Matrix R
forward inputs weights = sigmoid (mult inputs weights)

criterion :: Matrix R -> Matrix R -> Matrix R
criterion targets logits = (-1 * targets) * (log logits) - (1 - targets) * (log $ 1 - logits)

loss :: Matrix R -> Matrix R -> Int -> Double
loss targets logits batchSize = meanMatrix (criterion targets logits) batchSize

getGradient :: Matrix R -> Matrix R -> Matrix R -> Int -> Matrix R
getGradient inputs targets logits batchSize = (inputs * (logits - targets)) / (fromIntegral batchSize)

updateWeights :: Double -> Matrix R -> Matrix R -> Matrix R
updateWeights learningRate gradient weights = (weights - (learningRate `scale` gradient))

processGender :: [[[Char]]] -> IO (Matrix Gender)
processGender inputs = do
    return $ col [getGender x | x <- inputs]

processTargets :: [[[Char]]] -> IO (Matrix Survived)
processTargets inputs = do
    return $ col [getTarget x | x <- inputs]

getGender :: [[Char]] -> Gender
getGender row = case (row !! 5) of
    "male"   -> 0.0
    "female" -> 1.0

getTarget :: [[Char]] -> Survived
getTarget row = fromIntegral $ read $ row !! 1

readCSV :: String -> [[[Char]]]
readCSV file = [splitOn "," x | x <- (lines file)]

-- train :: Matrix R -> Matrix R -> Matrix R -> Int -> Matrix R
-- train inputs weights targets batchSize = getGradient targets logits inputs batchSize where
--     logits = forward inputs weights

getNewWeights :: Matrix R -> Matrix R -> Matrix R -> Int -> Matrix R
getNewWeights inputs targets weights batchSize =
    updateWeights 0.001 (getGradient inputs targets (forward inputs weights) batchSize) weights

runTrain :: Matrix R -> Matrix R -> State (Matrix R) (Matrix R)
runTrain inputs targets = do
    weights <- get
    put (getNewWeights inputs targets weights 891)
    return weights

main = do
    contents <- readFile "data/titanic/train.csv"
    weights <- rand 1 2
    inputs <- processGender $ tail (readCSV contents)
    targets <- processTargets $ tail (readCSV contents)
    print $ execState (runTrain inputs targets) weights

