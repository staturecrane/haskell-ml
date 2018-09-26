{-# LANGUAGE DataKinds                                #-}

module Main where
import Control.Monad.State                           (execState)
import Data.List.Split                               (splitOn)
import           Control.DeepSeq
import           Control.Exception.Base
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Data.Bitraversable
import           Data.IDX
import           Data.Maybe
import           Data.Traversable
import           Data.Tuple
import qualified Data.Vector                         as V
import qualified Data.Vector.Generic                 as VG
import qualified Data.Vector.Unboxed                 as VU
import           GHC.TypeLits
import qualified Numeric.LinearAlgebra               as HM  -- non-backprop hmatrix
import qualified Numeric.LinearAlgebra.Data          as HMD
import qualified Numeric.LinearAlgebra.Static        as HMS -- hmatrix with type-checked operations :D
import System.IO

import MNIST
import Neural
import Regression                                    (activate, forward, loss, runTrain)
import Titanic                                       (getAge, processFeatures, processTargets)

readCSV :: [Char] -> [[[Char]]]
readCSV file = [splitOn "," x | x <- (lines file)]

accuracy :: [Double] -> [Double] -> Int -> Double
accuracy targets predictions batchSize = (fromIntegral correct) / (fromIntegral batchSize) where
    correct = length $ filter (\idx -> targets !! idx == (activate predictions) !! idx) [0..batchSize-1]

main = do
    -- Logistic Regression
    --
    -- contents <- readFile "data/titanic/train.csv"
    -- let weights = matrix 1 [0.0 | _ <- [0..7]]
    -- let inputs = processFeatures $ tail (readCSV contents)
    -- let targets = processTargets $ tail (readCSV contents)

    -- -- subtract the header row
    -- let batchSize = (length $ toLists inputs) - 1
    -- let finalWeights = (execState (runTrain inputs targets batchSize) weights)
    -- let finalPredictions = forward inputs finalWeights

    -- print $ accuracy (concat $ toLists targets) (concat $ toLists finalPredictions) batchSize
    -- print $ loss targets finalPredictions batchSize

    -- MNIST Multilayer Perceptron
    --
    -- Just train <- loadMNIST "data/mnist/train-images-idx3-ubyte" "data/mnist/train-labels-idx1-ubyte"
    -- Just test  <- loadMNIST "data/mnist/t10k-images-idx3-ubyte"  "data/mnist/t10k-labels-idx1-ubyte"
    weightsInit <- HM.rand 728 300
    inputsInit <- HM.rand 1 728
    biasInit <- HM.rand 1 300
    yInit <- HM.rand 1 10
    let weights = HMS.matrix (concat $ HM.toLists weightsInit) :: HMS.L 728 300
    let inputs = HMS.matrix (concat $ HM.toLists inputsInit) :: HMS.L 1 728
    let bias = HMS.matrix (concat $ HM.toLists biasInit) :: HMS.L 1 300
    let y = HMS.vector (concat $ HM.toLists yInit) :: HMS.R 10
    print $ relu $ linear weights bias inputs

