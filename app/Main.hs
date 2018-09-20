module Main where

import Utils

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