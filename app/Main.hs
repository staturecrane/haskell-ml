module Main where

import Numeric.LinearAlgebra (Matrix, scale, R, (><))
import Numeric.LinearAlgebra.HMatrix (mul)

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

main :: IO ()
main = print (updateWeights 0.001 gradient weightMatrix) where
              weightMatrix = (2><2)
                             [ 0.01, 0.24,
                               0.15, 0.001 ] :: Matrix R
              gradient = calculateGradients (loss labels logits) where
              labels = (2><2)
                       [1.0, 0.0,
                        0.0, 1.0] :: Matrix R
              logits = forward inputMatrix weightMatrix where
                           inputMatrix = (2><2)
                             [ 1.0, 2.0,
                               6.0, 5.0 ] :: Matrix R

