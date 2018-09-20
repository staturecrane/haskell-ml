module Utils
  ( mult, 
    sigmoid,
    binaryCrossEntropy,
    loss,
    forward,
    calculateGradients,
    updateWeights,
    readCSV
  ) where

import Data.List.Split
import Numeric.LinearAlgebra (Matrix, scale, R, (><))
import Numeric.LinearAlgebra.HMatrix (mul)
import System.IO

mult :: Matrix R -> Matrix R -> Matrix R
mult inputs weights = (inputs `mul` weights)

sigmoid :: Matrix R -> Matrix R
sigmoid inputs = 1.0 / (1.0 + exp (-1 * inputs))

binaryCrossEntropy :: Matrix R -> Matrix R -> Matrix R
binaryCrossEntropy labels predictions = (-1 * (labels * (log predictions) + (1 - labels) * (log (1 - predictions))))

loss :: Matrix R -> Matrix R -> Matrix R
loss labels predictions = binaryCrossEntropy labels predictions

forward :: Matrix R -> Matrix R -> Matrix R
forward inputs weights = sigmoid (mult inputs weights)

calculateGradients :: Matrix R -> Matrix R
calculateGradients logits = logits * (1 - logits)

updateWeights :: Double -> Matrix R -> Matrix R -> Matrix R
updateWeights learningRate gradient weights = (weights - (learningRate `scale` gradient))

readCSV :: String -> [[[Char]]]
getLines file = [splitOn "," x | x <- (lines file)]