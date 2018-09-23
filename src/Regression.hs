module Regression where

import Control.Monad.State
import Numeric.LinearAlgebra                         (col, disp, Matrix, R, rand, scale, toLists, tr, Vector, (><))
import Numeric.LinearAlgebra.HMatrix                 (mul, sumElements)

meanMatrix :: Matrix R -> Int -> Double
meanMatrix inputs batchSize = (sumElements inputs) / (fromIntegral batchSize)

variance :: [Double] -> Double -> Double
variance inputs mean = sum [(x - mean)**2 | x <- inputs]

sigmoid :: Matrix R -> Matrix R
sigmoid inputs = 1.0 / (1.0 + exp (-1 * inputs))

forward :: Matrix R -> Matrix R -> Matrix R
forward inputs weights = sigmoid (mul inputs weights)

criterion :: Matrix R -> Matrix R -> Matrix R
criterion targets logits = (-1 * targets) * (log logits) - (1 - targets) * (log $ 1 - logits)

loss :: Matrix R -> Matrix R -> Int -> Double
loss targets logits batchSize = meanMatrix (criterion targets logits) batchSize

getGradient :: Matrix R -> Matrix R -> Matrix R -> Int -> Matrix R
getGradient inputs targets logits batchSize = (mul (tr inputs) (logits - targets)) / (fromIntegral batchSize)

updateWeights :: Double -> Matrix R -> Matrix R -> Matrix R
updateWeights learningRate gradient weights = (weights - (learningRate `scale` gradient))

getNewWeights :: Matrix R -> Matrix R -> Matrix R -> Int -> Matrix R
getNewWeights inputs targets weights batchSize =
    updateWeights 0.001 (getGradient inputs targets (forward inputs weights) batchSize) weights

runTrain :: Matrix R -> Matrix R -> Int -> State (Matrix R) (Matrix R)
runTrain inputs targets batchSize = do
    weights <- get
    forM_ [0..5000] $ \e -> do
        put (getNewWeights inputs targets weights batchSize)
    return weights

activate :: [Double] -> [Double]
activate predictions = [if x > 0.5 then 1.0 else 0.0 | x <- predictions ]
