module Titanic where

import Numeric.LinearAlgebra                         (col, disp, Matrix, R, rand, scale, toLists, tr, Vector, (><))
import Numeric.LinearAlgebra.HMatrix                 (mul, matrix, sumElements)
import Text.Read                                     (readMaybe)

type Gender = Double
type Age = Double
type SibSp = Double
type Embarked = Double
type PClass = Double
type Survived = Double

data Row = Row Gender Age PClass Embarked

processRow :: [[Char]] -> [Double]
processRow row = [gender, age] ++ pclass ++ embarked where
    gender = getGender row
    age = getAge row
    pclass = getPClass row
    embarked = getEmbarked row

processFeatures :: [[[Char]]] -> Matrix Double
processFeatures inputs = matrix 8 $ concat [processRow x | x <- inputs]

processGender :: [[[Char]]] -> Matrix Gender
processGender inputs = do
    col [getGender x | x <- inputs]

processTargets :: [[[Char]]] -> Matrix Survived
processTargets inputs = do
    col [getTarget x | x <- inputs]

getAge :: [[Char]] -> Age
getAge row =  case (readMaybe (row !! 6)) of
    -- 80 is the maximum age in the dataset. We divide the age by it to
    -- keep it between 0.0 and 1.0
    Just x -> x / 80
    Nothing -> 0.0

getPClass :: [[Char]] -> [PClass]
getPClass row =  case (row !! 2) of
    "1" -> [1.0, 0.0, 0.0]
    "2" -> [0.0, 1.0, 0.0]
    "3" -> [0.0, 0.0, 1.0]
    "" -> [0.0, 0.0, 0.0]

getEmbarked :: [[Char]] -> [Embarked]
getEmbarked row = case (readMaybe (row !! 12)) of
    Just x -> case x of
        "C" -> [1.0, 0.0, 0.0]
        "Q" -> [0.0, 1.0, 0.0]
        "S" -> [0.0, 0.0, 1.0]
    Nothing -> [0.0, 0.0, 0.0]

getGender :: [[Char]] -> Gender
getGender row = case (row !! 5) of
    "male"   -> -1.0
    "female" -> 1.0

getTarget :: [[Char]] -> Survived
getTarget row = fromIntegral $ read $ row !! 1

