module Main where

import Data.List.Split                               (splitOn)
import Control.Monad.State                           (execState)
import Numeric.LinearAlgebra                         (col, disp, Matrix, R, rand, scale, toLists, tr, Vector, (><))
import Numeric.LinearAlgebra.HMatrix                 (mul, matrix, sumElements)
import System.IO

import Regression                                    (activate, forward, loss, runTrain)
import Titanic                                       (getAge, processFeatures, processTargets)

readCSV :: [Char] -> [[[Char]]]
readCSV file = [splitOn "," x | x <- (lines file)]

accuracy :: [Double] -> [Double] -> Int -> Double
accuracy targets predictions batchSize = (fromIntegral correct) / (fromIntegral batchSize) where
    correct = length $ filter (\idx -> targets !! idx == (activate predictions) !! idx) [0..batchSize-1]

main = do
    contents <- readFile "data/titanic/train.csv"
    let weights = matrix 1 [0.0 | _ <- [0..7]]
    let inputs = processFeatures $ tail (readCSV contents)
    let targets = processTargets $ tail (readCSV contents)

    -- subtract the header row
    let batchSize = (length $ toLists inputs) - 1
    let finalWeights = (execState (runTrain inputs targets batchSize) weights)
    let finalPredictions = forward inputs finalWeights

    print $ accuracy (concat $ toLists targets) (concat $ toLists finalPredictions) batchSize
    print $ loss targets finalPredictions batchSize
